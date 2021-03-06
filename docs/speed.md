# Speeding up CVM

> At the bottom I'll place some todos for speed improvements

CVM was never intended to be 'fast' compared to C but it was meant to be relative. i.e. you could benchmark programs in CVM and expect to see a relative performance difference if you then benchmarked the same thing in C (with no optimisations of course).

Also I want the compiler to be fast ish but since the vast majority of programs will be small (< 5k lines total including libraries).  We also won't have that many optimisations (if any) thus lending ourselves quite a bit of leniancy in this regard.

Regardless we do use a few cool techniques to make CVM faster and some of them add some complexity so this document aims to address that.

## Speeding up the compiler

### Worker thread concurrency

> TLDR: We split the program into a series of jobs with some sync points so that we can spawn a ton of worker threads and allow the user to scale up the speed according to the number of threads they have to spare.

The compiler is split up into a series of phases that are completed in the following order

```
| Tokenization
↓ Parsing initial AST (!)
↓ Type inference (*)
↓ Type checking (*)
↓ Optimisations (if any)
↓ Transpilation (to C / IR Set)
```

Worker threads are just a series of jobs that are placed onto a queue for a thread to take.

Initial AST is a full program sync point (thus the '!') and this means that the worker threads won't go past this step until all files have been parsed into initial AST formats.

> The files are always processed like a stack this ensures that the deepest (and thus most likely least dependent file) will be processed first.

Type inference is an important sync step since it avoids having the classical issue of requiring another file to complete parsing to get types for.  Instead type inference is just lazy and utilises placeholder types in these cases (and actually will never go across the file boundary during this step because of how expensive it is to do - mutex wise).  This step doesn't even associate the placeholder/fresh types with values in the type table since this would require a lock of some sort (reader writer).

> The '*' indicates it only needs to wait until it's dependencies are compiled (not full program) so it could continue to next step immediately or have to wait for all.

After inference it does type checking, this associates the final types and will set them the importance of this is it uses a global fresh type table which uses a reader writer lock (meaning it is relatively efficient).  Importantly at this point the placeholder types aren't actually replaced at all!  Placeholder types will be replaced at the point of need inside the transpiler (for C) if it is run!

> However we do improve the efficiency by flattening the type table into a vector lookup and reduce the cycles that could exist (i.e. we reduce the case of `a -> b -> c -> int` and just reduce it to always `a -> int` - of course `b -> int` and `c -> int` always get reduced, this includes cases like `a -> b -> c` which get reduced to `a -> c` and of course b becomes `b -> c`).

## Type Definition Tables

We actually don't care about the type definitions till we get to type checking.

This even includes std types, they just are counted as 'var' types (which is basically just a type that has been given a name).

Even functions defined in external modules don't need a definition till type checking (what happens is we just say - oh the result will just be some 'fresh' placeholder type 'a' that we'll unify with the real type, same happens with args).

However regardless once we get to type checking and unification we need the definition table.  This is different to the type fresh table which holds what type each fresh placeholder type has.  The definition table holds a lot of meta data about types, importantly it fills structs in with padding (according to C rules) as well as associates types with their underlying representation.

There are 4 variants of types in this definition table.

- Integral Type; i.e. int, uint, usize, isize, ...
- Floating Pt Type; i.e. float, double, ...
- Type Aliases; i.e. 'typedef a int'
  - Interesting note: `*void` is actually a typedef for `*u8` this is because fundamentally they are identical (all casts to from u8 is valid just as much as void)
    - This is done because typedefs are implemented through a ParsedType -> TypeDefinition ruling, this allows us to express some other complex types simpler too
    - i.e. `int[..]` maps to `DynArray<int>`
    - Of course users don't get to do these weird defs

The table is indexed a little weirdly, basically it is ParsedType -> TypeDefinition, and ParsedType's are hashed according to;

- ParsedType::Var just hashes it's name
- ParsedType::Ptr hashes it's inner value
  - Yes this means that void* clashes with void
    - Turns out that we have very few ptr types so this isn't really a problem (hash collisions happen anyways)
- ParsedType::Array just hashes the inner value
  - Same problems as Ptr of course, this also means that it ignores the constant factor
- ParsedType::DynArray just hashes inner value
- ParsedType::Fresh hashes fresh value
  - Technically this really should never happen so it currently produces a warning as well
- ParsedType::Func hashes each of its args and ret and combines them
  - Highly expensive hash, never use this please, produces a warning upon use.
    - Hash is expensive since it needs to hash and combine every single argument type and the return type!
- ParsedType::Unknown produces an error

## Speeding up the interpreter

Not yet built so eh ?

## Speeding up the frontend

Not yet built so eh ?

## Speed Improvements


