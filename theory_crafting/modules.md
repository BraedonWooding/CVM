# Theory Crafting - Modules

Modules are CVM's solution to multiple source files and partial includes, currently all files belong to the same 'module' and there is no way to selectively choose features (for example the entire std lib is included by default).

This would enable you to build libraries easier as well.

## Simple Example

```c
// mylib.cvm
module MyLib;

struct MyList<T> {
  next: *MyList<T>,
  item: T
}

// main.cvm
// - by default all files are part of module main
module main;

// this imports MyLib and allows use of it's objects
// they aren't scoped though!
use my_lib;

fn main(int argc, char **argv) -> int {
  list: *MyList = null;
  for i := 0; i < argc; i += 1 {
    item := new { next: list, item: argv[i] };
    list = item;
  }

  return 0;
}
```

## Specifics

- To maintain backwards compatability all files by default have module 'Main'.  Only one module may have a 'main' function also (doesn't have to be the 'Main' module).
- Modules scope declarations by default but by default all std library modules define `module use std.sub_library` which automatically pushes into scope
  - In the future we'll deprecate this and most likely introduce a pattern system like `use std.io.*` or similar
- To enable a scoping declaration you just declare it as `module my_lib` this would prefix all members with `my_lib` i.e. `my_lib_MyList`.
  - The prefixing is a C / bytecode translation but in reality you access it through the member operator i.e. `my_lib.MyList`
- You import modules with `use`, by default all modules should be installed under `usr/cvm_modules` for system based modules or in a `cvm_modules` folder in the same directory that the cvm run command is ran.
  - You can add extra module paths via `--module-path ~/my_extra_modules/` option
- You can have multiple modules in a single file
- You can define sub modules through the use of `.` i.e. `module std.io`

## Changes to parser

A new AST node structured as

```ebnf
Module ::= 'module' 'use'? MyLib
```

Also a member access no longer has to refer to a variable it can also refer to a module.

> This also is impacted by the struct changes to enable member functions since in those cases the member access no longer has to refer to a variable and can also refer to a struct type.

## Backwards Compatability

As stated previously all code that compiled with previous versions of CVM will continue to work no breaking changes are made.

However code that compiles with this module won't work on previous versions.

However not using a module 'use' for something in core or the C standard library is now a warning (will still continue to compile) for example a simple hello world;

```c
fn main(void) {
  printf("Hello World\n");
}
```

Will now produce a warning and require `use std.io` at the top to silence the warning.

## Naming convention for modules

Modules should be lower camel case and the compiler should raise a naming warning if `--style` is passed as a flag and a non comforming module name is defined.

## Std Naming for sub modules

Standard library modules will just be `std.<name>` where `<name>` is their standard name but won't repeat `std` twice i.e. `std.arg`, `std.errno`, `std.assert`, `std.threads`, `std.align`, `std.def`, ... and so on.

> Note: only a subset of the C standard library is currently supported by CVM see the file [std.md](std.md) if you wish to find out which ones are currently supported.
