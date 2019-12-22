use crate::compiler::ast::*;
use crate::compiler::lexer::*;
use crate::compiler::parser::*;

extern crate log;
use log::{info, trace, warn};

pub struct Transpiler {
    depth: usize,
    builder: String,
}

impl Transpiler {
    pub fn new() -> Transpiler {
        Transpiler { depth: 0, builder: String::from("") }
    }

    pub fn get_output<'a>(&'a self) -> &'a str {
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
        self.depth += 1;
        self.write_indent();
        self.builder += symbol;
    }

    fn transpile_statement(&mut self, statement: &Statement) {
        
    }

    fn transpile_func_decl(&mut self, decl: &Function) {
        
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

    fn transpile_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Assign{lhs, rhs, kind} => {
                self.transpile_expr(&lhs);
                self.transpile_assignment_op(&kind);
                self.transpile_expr(&rhs);
            },
            ExprKind::Decl(decl) => {
                self.transpile_decl(&decl);
            },
            ExprKind::New(..) => {
                // TODO
            },
            ExprKind::Unary(kinds, expr) => {
                for kind in kinds.as_slice() { self.transpile_unary_op(&kind); }
                self.transpile_expr(&expr);
            },
            ExprKind::Paren(inner) => {
                self.builder += "(";
                self.transpile_expr(&inner);
                self.builder += ")";
            },
            ExprKind::Var(id) => self.builder += &id,
            ExprKind::Member(expr, ident) => {
                self.transpile_expr(&expr);
                self.builder += &format!(".{}", ident);
            },
            ExprKind::GenFuncCall(..) => {
                // TODO
            },
            ExprKind::FuncCall(expr, args) => {
                self.transpile_expr(&expr);
                self.builder += "(";
                for (i, arg) in args.iter().enumerate() {
                    self.transpile_expr(&arg);
                    if i < args.len() - 1 { self.builder += ", " }
                }
                self.builder += ")";
            },
            ExprKind::Cast{..} => {
                // TODO
            },
            ExprKind::Index(obj, index) => {
                self.transpile_expr(&obj);
                self.builder += "[";
                self.transpile_expr(&index);
                self.builder += "]";
            },
            ExprKind::Sizeof(..) => {
                // TODO
            },
            ExprKind::Binop(lhs, op, rhs) => {
                self.transpile_expr(&lhs);
                self.transpile_binop_op(&op);
                self.transpile_expr(&rhs);
            },
            ExprKind::Ternary{..} => {
                // TODO
            },
            ExprKind::Constant(val) => {
                match val {
                    ConstantKind::Int32(n) => self.builder += &format!("{}", n),
                    ConstantKind::Flt64(n) => self.builder += &format!("{}", n),
                    ConstantKind::Str(string) => self.builder += &string,
                    ConstantKind::Char(c) => self.builder += &format!("{}", c),
                    ConstantKind::Null => self.builder += "NULL",
                    ConstantKind::Bool(b) => self.builder += &format!("{}", b),
                };
            },
            ExprKind::Let(expr) => {
                // just perform the evaluation of expr
                // since in C you can use assignments as conditionals
                self.transpile_expr(&expr);
            },
            ExprKind::Lambda(lambda) => {
                // this will need to be declared else where...
                // TODO
            },
            ExprKind::Uninitialiser => {
                // no op...
                // NOTE: TODO (BW): we should instead do something if this doesn't exist
                //                  for a given type... all decls should be default init
            }
        }
    }

    // just write the lhs of the type
    // i.e. for C type => int (*a3)[8]
    // it'll just write the int (*
    fn transpile_type_lhs(&mut self, ty: &Type) {
        match ty {
            Type::Array{inner, ..} => {
                self.transpile_type_lhs(&inner);
                // no other logic
            },
            Type::Pointer(inner) => {
                self.transpile_type_lhs(&inner);
                match **inner {
                    Type::Var{..} | Type::Pointer(..) | Type::Fresh{..} | Type::Func{..} => {
                        // simple type we just add a '*' afterwards
                        self.builder += "*";
                    },
                    _ => {
                        // else we have a complex type (array)
                        // so we need to wrap the type in a '('
                        self.builder += "(*"
                    }
                }
            },
            Type::Var{id, gen_args} => {
                // NOTE: Ignoring gen args for now..
                if gen_args.len() > 0 {
                    warn!("Generic args on structs aren't supported for C transpilation yet...");
                }
                self.builder += &id;
            },
            Type::Func{name, args: _, ret, ..} => {
                let name = match name {
                    Some(_) => &name.as_ref().unwrap(),
                    None => "func_TODO_NO_NAME"
                };

                match ret {
                    Some(ty) => self.transpile_type_lhs(&ty),
                    None => {
                        warn!("No return type for function {}", name);
                        self.builder += "???";
                    }
                }
                self.builder += &format!("(*{0}", name);
            },
            Type::Fresh{..} => {
                warn!("Fresh variables not supported for printing yet");
                self.builder += "???";
            }
        }
    }

    pub fn transpile_type(&mut self, ty: &Type) {
        self.transpile_type_lhs(&ty);
        self.transpile_type_rhs(&ty);
    }

    // NOTE: we do inner after ourselves in rhs
    fn transpile_type_rhs(&mut self, ty: &Type) {
        match ty {
            Type::Array{inner, len} => {
                self.builder += "[";
                self.transpile_expr(len);
                self.builder += "]";
                self.transpile_type_rhs(&inner);
                // just chuck the []
            },
            Type::Pointer(inner) => {
                match **inner {
                    Type::Var{..} | Type::Pointer(..) | Type::Fresh{..} | Type::Func{..} => {
                        // simple types are no-ops
                    },
                    _ => {
                        // else we have a complex type (array)
                        // so we need to close the (*
                        self.builder += ")";
                    }
                }
                self.transpile_type_rhs(&inner);
            },
            Type::Var{..} => { /* no op */ },
            Type::Func{name, args, ret, ..} => {
                self.builder += ")(";
                if args.len() > 0 {
                    for (i, arg) in args.iter().enumerate() {
                        self.transpile_type_lhs(arg);
                        self.transpile_type_rhs(arg);
                        if i < args.len() - 1 { self.builder += ","; }
                    }
                } else {
                    self.builder += "void";
                }
                self.builder += ")";
                match ret {
                    Some(ty) => self.transpile_type_rhs(&ty),
                    None => {
                        warn!("No return type for function {:?}", name);
                        self.builder += "???";
                    }
                }
            },
            Type::Fresh{..} => { }
        }
    }

    fn transpile_decl(&mut self, decl: &Decl) {
        // write it as decl_type id val
        match &decl.decl_type {
            Some(ty) => {
                self.transpile_type_lhs(&ty);
            },
            None => {
                warn!("decl type for decl with id {} is none... replacing type with ???", decl.id);
                self.builder += "??? ";
            }
        }

        self.builder += &decl.id;
        if decl.decl_type.is_some() {
            self.transpile_type_rhs(&decl.decl_type.as_ref().unwrap());
        }
        // values are ignored... since they are just inserted
        // at the construction site!
    }

    fn transpile_struct_decl(&mut self, decl: &Struct) {
        self.builder += &format!("typedef struct {0} {0}\nstruct {0}",
                                decl.id);
        self.begin_scope(" {\n");
        // generic args not yet supported... TODO
        if decl.gen_args.len() > 0 {
            warn!("Generic args on structs aren't supported for C transpilation yet...");
        }
        // NOTE: 'is' list is ignored for transpilation

        for member in decl.decls.as_slice() {
            self.transpile_decl(&member);
            self.builder += ";\n";
        }
        self.end_scope("}");
    }

    fn transpile_top_level(&mut self, top_level: &TopLevel) {
        match top_level {
            TopLevel::StructDecl(decl) => self.transpile_struct_decl(&decl),
            TopLevel::FuncDecl(decl) => self.transpile_func_decl(&decl)
        }
    }

    fn transpile_block(&mut self, block: &Block) {
        
    }

    pub fn transpile_program(&mut self, program: &Program) {
        for statement in program.top_level.as_slice() {
            self.transpile_top_level(&statement);
            self.builder += "\n\n";
        }
    }
}
