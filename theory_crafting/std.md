# Theory Crafting - Standard Library (std)

Note: CVM is not a fully compliant C compiler and the std is not a fully compliant C standard library mainly due to the fact it reimplements some things as generics.

## Posix Support

Posix is currently not supported by CVM but this won't last forever there are plans to eventually support it.

A different theory crafting section will be made for posix support.

## Libraries that are fully supported

Note that macros are always implemented as constants.

### `assert.h`

Assert is changed and actually prints out the evaluated value of each section of the expression (this is generated at compile time so don't worry about the cost especially since assert causes an abort - it is also eagerly done and won't double evaluate things) i.e. `assert(x + y == 4)` would print out;

```c
Assert Failed LineNo:ColNo
LineNo | x + y == 4
Where x := <value>, y := <value>
Therefore x + y := <value>
```

### `stdbool.h` and `inttypes.h`/`stdint.h`

Already pre-installed and supported, however `_Bool` isn't allowed (just use `bool`) also any associated macros aren't supported.

### The rest (simple full supports)

- `ctype.h`
- `errno.h`
- `float.h`
- `limits.h`
- `math.h` (note: doesn't require `-lm`)
- `stddef.h`
  - All supported types are already defined only the limits come with this include.
- `stdio.h`
  - Printf any some others are generic
- `stdlib.h`
  - Malloc exists but you should use 'new'
- `string.h`

## Libraries likely to receive support soon

- `fenv.h`
- `locale.h`
- `signal.h`
- `stdnoreturn.h`
- `time.h`
- `uchar.h`
- `wchar.h`
- `wctype.h`

## Libraries likely to receive support through a different way

### `stdalign.h`

Will be supported through a new core function similar to `sizeof` the only difference is that it's signature `fn<T = void> alignas(expr: usize = 0)`.

A new function `alignof<T>(expr: T = new {})` will also be added which is similar to `sizeof` but returns the alignment of the object not the size.
  
> Note the signature requires default generic types which is a different theory craft but can be implemented without it being implemented

If you implement both then the following condition must hold `expr == alignof(T)` else it'll cause a runtime error (or compile time if seen at compile time)

### `stdarg.h`

> Currently stdarg is only supported in std this part would allow it in user land.

Will have type checking on it's methods and va_start and va_end are automatically defined for any va arg function.

> For example printf now has format `fn printf(fmt: *const char, args: ...)`

`args` represents an array and to retrieve an element you just grab and cast (it'll automatically type check) i.e. `cast.<int>(args)`.  `args` has no length to it (to maintain C compatability) so you'll have to derive it from the other argument.

If no other argument is given then a dummy one is created but it'll produce a warning if you try to access any of the members (how could you know how many you should have?)

We should probably add a compile time feature to dictate types for va args.

You can copy by just doing `cloned := args;` (under the scenes will do a va_copy which is almost certaintly just a pointer copy).

## Libraries likely to receive support in the Posix support step

- `stdatomic.h`, `threads.h` for the same reason.  I don't want to support concurrency / threading / multi processing until the visualiser supports it.
  - You'll probably be able to choose between `threads.h` and `pthreads.h`
- `complex.h`

## Libraires unlikely to receive support

- `iso646.h` due to it fundamentally changing how parsing would have to work.  Digraphs/Trigraphs are also not supported for a similar reason.
- `setjmp.h` this breaks so many things since it's so unsafe (i.e. defer is a big one).
  - This may receive some support but it'll probably just end up being an exact C replacement and no safety around defers and signal based things.
- `tgmath.h` by default `math.h` already uses generic functions
