# Macros in CVM

There are a few useful Macros built in CVM using Rust's macro system.

Primarily they are intended to be used for tests but they do have some uses outside that.

## `create_type!`

This allows you to create extremely complicated nested types very simply, the downside is that it makes copies of all variable strings given and can end up doing some conversion costs at runtime.

A few examples;

### Complex Function Example

```rust
// Note: constant! is only available currently in tests
//       it will be moved in the future
create_type!(Func (Var "T"), (Array [constant!(Int 10)] (Pointer (Var "char"))) -> (Fresh a))

// Compared to (I could make this nicer ish but no where near enough to make it usable...)
let sample = ParsedType::Func{args: vec![ParsedType::Var{id: Spanned::new("T".to_string(), Span::default()), gen_args: vec![]}, ParsedType::Array{inner: Box::new(ParsedType::Pointer(Box::new(ParsedType::Var{id: Spanned::new("char".to_string(), Span::default()), gen_args: vec![]}))), len: Box::new(Expr{kind: Spanned::new(ExprKind::Constant(ConstantKind::Int32(10)), Span::default()), type_annot: ParsedType::Var{id: Spanned::new("int".to_string(), Span::default()), gen_args: vec![])}})}]), ret: Box::new(ParsedType::Fresh{id: string_to_fresh_id("a")})};
// Note how 'a' gets converted using string_to_fresh_id...
// Since it is always constant it mayyy get compiled out during release
// but I wouldn't rely on it
```

The best way to utilise this is in 2 cases

### Tests

For example almost all tests utilise this extensively (as talked about in that section).  It's really useful since we can express really complicated operations simply.

> i.e. Currently in about 20 lines of code the tests generate around 300 lines of rust code that are equivalent this is a huge deal.

### Primative types

A common pattern in the compiler is something like; `create_type!(Var "size_t")` this allows for us to create primative types for operations such as sizeof, boolean evaluations (i.e. conditions in if/while/for) and other such things.

## `constant!`

Allows you to create a constant, quite efficient but not very useful outside of tests.

All constants create an Expr that consists of a ConstantKind.

### Examples

Each type (bar `Null` is followed by a literal of that type i.e. `Str` is followed by something like `"hey"`, `Int` is followed by an integer like `2`);

- `Str`, `Int`, `Double`, `Char`, `Null`, `Bool`
