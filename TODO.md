# TODO List

## Frontend

- Everything (lol)

## Backend

### Overall

- Splitting the C / Mips compilers into separate modules / crates
- Supplying stuff like Ident as common crate items rather than reimplementing them

### C Module

- Needs to be a separate crate
- Optimiser
- Byte code transpiler
- C -> Mips transpiler too (since it's very simple to transpile CVM bytecode <-> MIPS)

### Mips Module

- Separate crate
- SPIM support
- MIPS support in interpreter (so we don't have to do weird stuff with registers)
  - I do still like CVM bytecode support but when producing MIPS code from C I would prefer to not have to chuck that back through a CVM parser... to execute it.  It also would make life a lot easier
