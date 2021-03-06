use crate::*;
use ast::*;
use compiler::*;

extern crate log;
use log::warn;

use std::collections::hash_map::HashMap;

pub struct Transpiler<'a> {
    depth: usize,
    builder: String,
    alpha_types: bool,
    fresh_type_lookup: HashMap<usize, String>,
    type_definition_table: &'a TypeDefinitionTable,
}

bitflags! {
    /// Options for transpiling a type
    pub struct TypeOpts: u8 {
        /// We are transpiling a function return
        /// So convert arrays to pointers
        const FUNCTION_RETURN = 0x1;
        /// We are parsing a non simple type parent
        /// So add spaces to simple var types
        const SIMPLE_VAR_SPACE = 0x2;
    }
}

impl<'a> Transpiler<'a> {
    pub fn new(
        alpha_types: bool,
        type_definition_table: &'a TypeDefinitionTable,
    ) -> Transpiler<'a> {
        Transpiler {
            depth: 0,
            builder: String::from(""),
            alpha_types,
            fresh_type_lookup: HashMap::default(),
            type_definition_table,
        }
    }

    pub fn get_output<'b>(&'b self) -> &'b str {
        &self.builder
    }

    fn write_indent(&mut self) {
        self.builder += &"    ".repeat(self.depth);
    }

    fn begin_scope(&mut self, symbol: &str) {
        self.depth += 1;
        self.builder += symbol;
    }

    fn end_scope(&mut self, symbol: &str) {
        self.depth -= 1;
        self.write_indent();
        self.builder += symbol;
    }

    fn transpile_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::If {
                if_cond,
                if_block,
                else_if,
                else_block,
            } => {
                self.builder += "if (";
                self.transpile_expr(&if_cond);
                self.builder += ") ";
                self.transpile_block(&if_block);

                for (expr, block) in else_if {
                    self.builder += " else if (";
                    self.transpile_expr(&expr);
                    self.builder += ") ";
                    self.transpile_block(&block);
                }

                if else_block.is_some() {
                    self.transpile_block(&else_block.as_ref().unwrap());
                }
                self.builder += "\n";
            }
            Statement::While(expr, block) => {
                self.builder += "while (";
                self.transpile_expr(&expr);
                self.builder += ") ";
                self.transpile_block(&block);
                self.builder += "\n";
            }
            Statement::For(init, cond, step, block) => {
                self.builder += "for (";
                if init.is_some() {
                    self.transpile_expr(&init.as_ref().unwrap());
                }
                self.builder += ";";
                if cond.is_some() {
                    self.builder += " ";
                    self.transpile_expr(&cond.as_ref().unwrap());
                }
                self.builder += ";";
                if step.is_some() {
                    self.builder += " ";
                    self.transpile_expr(&step.as_ref().unwrap());
                }
                self.builder += ") ";
                self.transpile_block(&block);
                self.builder += "\n";
            }
            Statement::Defer => {
                // NOTE: Defers are just ignored
            }
            Statement::Expr(expr) => {
                self.transpile_expr(&expr);
                self.builder += ";";
            }
            Statement::Return(expr) => {
                self.builder += "return ";
                self.transpile_expr(&expr);
                self.builder += ";";
            }
        }
    }

    fn transpile_func_decl(&mut self, decl: &Function) {
        let opts = TypeOpts::FUNCTION_RETURN | TypeOpts::SIMPLE_VAR_SPACE;
        self.transpile_type(&decl.ret, opts);
        self.builder += &format!("{}(", *decl.id);
        // write arguments
        for (i, arg) in decl.args.iter().enumerate() {
            self.transpile_decl(&arg);
            if i < decl.args.len() - 1 {
                self.builder += ", ";
            }
        }
        self.builder += ") ";
        self.transpile_block(&decl.block);
        self.builder += "\n";
    }

    fn transpile_assignment_op(&mut self, op: &AssignmentKind) {
        self.builder += &match op {
            AssignmentKind::Assign => " = ",
            AssignmentKind::MulAssign => " *= ",
            AssignmentKind::DivAssign => " /= ",
            AssignmentKind::ModAssign => " %= ",
            AssignmentKind::AddAssign => " += ",
            AssignmentKind::SubAssign => " -= ",
            AssignmentKind::ShiftLeftAssign => " <<= ",
            AssignmentKind::ShiftRightAssign => " >>= ",
            AssignmentKind::BitAndAssign => " &= ",
            AssignmentKind::BitXorAssign => " ^= ",
            AssignmentKind::BitOrAssign => " |= ",
        }
    }

    fn transpile_binop_op(&mut self, op: &BinopKind) {
        self.builder += &match op {
            BinopKind::BitOr => " | ",
            BinopKind::BitAnd => " & ",
            BinopKind::BitXor => " ^ ",
            BinopKind::BoolAnd => " && ",
            BinopKind::BoolOr => " || ",
            BinopKind::Equal => " == ",
            BinopKind::NotEqual => " != ",
            BinopKind::LessThan => " < ",
            BinopKind::GreaterThan => " > ",
            BinopKind::LessEqual => " <= ",
            BinopKind::GreaterEqual => " >= ",
            BinopKind::ShiftRight => " >> ",
            BinopKind::ShiftLeft => " << ",
            BinopKind::Add => " + ",
            BinopKind::Sub => " - ",
            BinopKind::Mul => " * ",
            BinopKind::Div => " / ",
            BinopKind::Mod => " % ",
        }
    }

    fn transpile_unary_op(&mut self, op: &UnaryKind) {
        self.builder += &match op {
            UnaryKind::BitNot => "~",
            UnaryKind::Not => "!",
            UnaryKind::Deref => "*",
            UnaryKind::Address => "&",
            UnaryKind::Pos => "+",
            UnaryKind::Neg => "-",
        }
    }

    fn transpile_initialiser_list(&mut self, vals: &[Initialiser]) {
        for val in vals.iter() {
            self.transpile_initialiser(val);
            self.builder += ", "; // add's superfluous comment...
            // TODO: ^^ Fix for singular values and have flag for
            //       non singular (i.e. (int) { 1 } compared to
            //       (int) { 1, } and ... so on)
        }
    }

    fn transpile_initialiser(&mut self, init: &Initialiser) {
        match init {
            Initialiser::Key{key, val: vals} => {
                self.builder += &format!(".{} = ", **key);
                // Technically we don't require braces around scalar initialiser
                // and most compilers warn if they are defined (sadly)
                // (they aren't illegal in C though and are technically fine)
                // however I don't currently care about fixing this since it
                // requires us to do a bunch of random type stuff which I don't
                // really wanna do rn, this can fixed later @TODO @SCALAR_INIT
                self.builder += "{ ";
                self.transpile_initialiser_list(vals);
                self.builder += " }";
            },
            Initialiser::Val{val} => {
                self.transpile_expr(val);
            }
        }
    }

    fn transpile_expr(&mut self, expr: &Expr) {
        match &*expr.kind {
            ExprKind::Assign { lhs, rhs, kind } => {
                self.transpile_expr(&lhs);
                self.transpile_assignment_op(&kind);
                self.transpile_expr(&rhs);
            }
            ExprKind::Decl(decl) => {
                self.transpile_decl(&decl);
                match &decl.val {
                    Some(Expr {
                        kind:
                            Spanned {
                                inner: ExprKind::Uninitialiser,
                                ..
                            },
                        ..
                    }) => {}
                    Some(expr) => {
                        self.builder += " = ";
                        self.transpile_expr(&expr);
                    }
                    None => {
                        self.builder += " = {0}";
                    }
                }
            }
            ExprKind::Init(ty, inits) => {
                let ty = if let ParsedType::Pointer(_) = ty {
                    warn!("Can't initialise pointers due to potential misunderstanding around mallocs");
                    // TODO: This shouldn't exist here...
                    self.builder += "???";
                    return;
                } else {
                    ty
                };

                // We can just do a standard initialisation
                // @TODO: We can also fix the case of scalar initialisers
                //        here by just writing the literal instead of
                //        doing a bizarre case
                // HOWEVER: this isn't always true...
                //          for example you can take a reference to a 'new'
                //          / scalar initaliser but not to a literal value
                // @SCALAR_INIT
                self.builder += "(";
                self.transpile_type(ty, TypeOpts::empty());
                self.builder += ") { ";
                self.transpile_initialiser_list(inits);
                self.builder += " }";
            }
            ExprKind::Unary(kinds, expr) => {
                for kind in kinds.as_slice() {
                    self.transpile_unary_op(&kind);
                }
                self.transpile_expr(&expr);
            }
            ExprKind::Paren(inner) => {
                self.builder += "(";
                self.transpile_expr(&inner);
                self.builder += ")";
            }
            ExprKind::Var(id) => self.builder += &id,
            ExprKind::Member(expr, ident) => {
                self.transpile_expr(&expr);
                self.builder += &format!(".{}", **ident);
            }
            ExprKind::GenFuncCall(..) => {
                // TODO
                self.builder += "???";
            }
            ExprKind::FuncCall(expr, args) => {
                self.transpile_expr(&expr);
                self.builder += "(";
                for (i, arg) in args.iter().enumerate() {
                    self.transpile_expr(&arg);
                    if i < args.len() - 1 {
                        self.builder += ", "
                    }
                }
                self.builder += ")";
            }
            ExprKind::Cast { to, obj, .. } => {
                // we are going to write it as (to)(obj)
                // this will just make sure no nasty precedence exists
                self.builder += "(";
                self.transpile_type(to, TypeOpts::empty());
                self.builder += ")(";
                self.transpile_expr(&obj);
                self.builder += ")";
            }
            ExprKind::Index(obj, index) => {
                self.transpile_expr(&obj);
                self.builder += "[";
                self.transpile_expr(&index);
                self.builder += "]";
            }
            ExprKind::Sizeof(ty, ..) => {
                // always choose the type
                self.builder += "sizeof(";
                self.transpile_type(&ty, TypeOpts::empty());
                self.builder += ")";
            }
            ExprKind::Binop(lhs, op, rhs) => {
                self.transpile_expr(&lhs);
                self.transpile_binop_op(&op);
                self.transpile_expr(&rhs);
            }
            ExprKind::Ternary { .. } => {
                // TODO
                self.builder += "???";
            }
            ExprKind::Constant(val) => {
                match val {
                    ConstantKind::Int32(n) => self.builder += &format!("{}", n),
                    ConstantKind::Flt64(n) => self.builder += &format!("{}", n),
                    ConstantKind::Str(string) => {
                        self.builder += "\"";
                        self.builder += &string.escape_default().to_string();
                        self.builder += "\"";
                    }
                    ConstantKind::Char(c) => self.builder += &format!("{}", c),
                    ConstantKind::Null => self.builder += "NULL",
                    ConstantKind::Bool(b) => self.builder += &format!("{}", b),
                };
            }
            ExprKind::Let(expr) => {
                // just perform the evaluation of expr
                // since in C you can use assignments as conditionals
                self.transpile_expr(&expr);
            }
            ExprKind::Lambda(_lambda) => {
                // this will need to be declared else where...
                // TODO
                self.builder += "???";
            }
            ExprKind::Uninitialiser => {
                // no op...
                // NOTE: TODO (BW): we should instead do something if this doesn't exist
                //                  for a given type... all decls should be default init
            }
        }
    }

    fn get_raw_type(&self, ty: &ParsedType) -> ParsedType {
        if let ParsedType::Unknown = ty {
            return ParsedType::Unknown;
        }

        match self.type_definition_table.get(ty) {
            Some(TypeDefinition::Alias(id)) => {
                // ewww TODO: awful feeling clone
                self.get_raw_type(&create_type!(Var id))
            }
            Some(TypeDefinition::FloatingPt { c_name, .. })
            | Some(TypeDefinition::Integral { c_name, .. }) => create_type!(Var c_name),
            None => ty.clone(),
        }
    }

    // just write the lhs of the type
    // i.e. for C type => int (*a3)[8]
    // it'll just write the int (*
    fn transpile_type_lhs(&mut self, ty: &ParsedType, opts: TypeOpts) {
        let is_func_ret = opts.contains(TypeOpts::FUNCTION_RETURN);
        match ty {
            ParsedType::Array { inner, .. } if !is_func_ret => {
                self.transpile_type_lhs(&inner, opts | TypeOpts::SIMPLE_VAR_SPACE);
            }
            ParsedType::Pointer(inner) | ParsedType::Array { inner, .. } => {
                self.transpile_type_lhs(&inner, opts | TypeOpts::SIMPLE_VAR_SPACE);

                match **inner {
                    ParsedType::Array { .. } if !is_func_ret => {
                        // array type so we need to wrap the type in a '('
                        self.builder += "(*";
                    }
                    _ => {
                        // non array type we just add a '*' afterwards
                        self.builder += "*";
                    }
                }
            }
            ParsedType::Var { id, gen_args } => {
                // NOTE: Ignoring gen args for now..
                if gen_args.len() > 0 {
                    warn!("Generic args on structs aren't supported for C transpilation yet...");
                }
                self.builder += &id;
                if opts.contains(TypeOpts::SIMPLE_VAR_SPACE) {
                    self.builder += " ";
                }
            }
            ParsedType::Func { ret, .. } => {
                let opts = opts | TypeOpts::SIMPLE_VAR_SPACE | TypeOpts::FUNCTION_RETURN;
                self.transpile_type_lhs(&ret, opts);
                self.builder += &format!("(*");
            }
            ParsedType::Fresh { id } => {
                let use_alpha = self.alpha_types;
                let count = self.fresh_type_lookup.len();
                let cpy = self
                    .fresh_type_lookup
                    .entry(*id)
                    .or_insert_with(|| Self::generate_fresh(use_alpha, count));
                self.builder.push_str(cpy);

                if opts.contains(TypeOpts::SIMPLE_VAR_SPACE) {
                    self.builder += " ";
                }
            }
            ParsedType::Unknown => {
                self.builder += "???";
                if opts.contains(TypeOpts::SIMPLE_VAR_SPACE) {
                    self.builder += " ";
                }
            }
        }
    }

    fn generate_fresh(alpha_types: bool, id: usize) -> String {
        if alpha_types {
            // generate type name as a, b, c, ..., aa, ..., abc
            // and so on...
            fresh_id_to_string(id)
        } else {
            // most compilers support '$' as an identifier token
            // but this isn't why it is used here, it's used here
            // to distinguish itself from other identifiers to avoid
            // name collision
            format!("$_{}", id)
        }
    }

    /// Transpile a type with given opts
    pub fn transpile_type(&mut self, ty: &ParsedType, opts: TypeOpts) {
        let ty = self.get_raw_type(ty);
        self.transpile_type_lhs(&ty, opts);
        self.transpile_type_rhs(&ty, opts);
    }

    // NOTE: we do inner after ourselves in rhs
    fn transpile_type_rhs(&mut self, ty: &ParsedType, opts: TypeOpts) {
        let is_func_ret = opts.contains(TypeOpts::FUNCTION_RETURN);
        match ty {
            ParsedType::Array { inner, len } if !is_func_ret => {
                self.builder += "[";
                self.transpile_expr(len);
                self.builder += "]";
                self.transpile_type_rhs(&inner, opts);
                // just chuck the []
            }
            ParsedType::Pointer(inner) | ParsedType::Array { inner, .. } => {
                if let ParsedType::Array { .. } = **inner {
                    if !is_func_ret {
                        self.builder += ")";
                    }
                }
                self.transpile_type_rhs(&inner, opts);
            }
            ParsedType::Var { .. } | ParsedType::Fresh { .. } | ParsedType::Unknown => { /* no op */
            }
            ParsedType::Func { args, ret, .. } => {
                self.builder += ")(";
                if !args.is_empty() {
                    for (i, arg) in args.iter().enumerate() {
                        self.transpile_type(&arg, opts & !TypeOpts::SIMPLE_VAR_SPACE);
                        if i < args.len() - 1 {
                            self.builder += ", ";
                        }
                    }
                } else {
                    self.builder += "void";
                }
                self.builder += ")";
                self.transpile_type_rhs(&ret, opts | TypeOpts::FUNCTION_RETURN);
            }
        }
    }

    fn transpile_decl(&mut self, decl: &Decl) {
        // write it as decl_type id val
        let ty = self.get_raw_type(&decl.decl_type);
        self.transpile_type_lhs(&ty, TypeOpts::SIMPLE_VAR_SPACE);
        self.builder += &decl.id;
        self.transpile_type_rhs(&ty, TypeOpts::SIMPLE_VAR_SPACE);
        // values are ignored... since they are just inserted
        // at the construction site!
    }

    fn transpile_struct_decl(&mut self, decl: &Struct) {
        self.builder += &format!("typedef struct {0} {0};\nstruct {0}", *decl.id);
        self.begin_scope(" {\n");
        // generic args not yet supported... TODO
        if decl.gen_args.len() > 0 {
            warn!("Generic args on structs aren't supported for C transpilation yet...");
        }
        // NOTE: 'is' list is ignored for transpilation

        for member in decl.decls.as_slice() {
            self.write_indent();
            self.transpile_decl(&member);
            self.builder += ";\n";
        }
        self.end_scope("}\n");
    }

    fn transpile_block(&mut self, block: &Block) {
        self.begin_scope("{\n");
        for statement in block.statements.as_slice() {
            self.write_indent();
            self.transpile_statement(statement);
            self.builder += "\n";
        }
        self.end_scope("}");
    }

    pub fn transpile_program(&mut self, program: &Program) {
        for decl in program.structs.values() {
            self.transpile_struct_decl(decl);
            self.builder += "\n";
        }

        for decl in program.functions.values() {
            self.transpile_func_decl(decl);
            self.builder += "\n";
        }
    }
}
