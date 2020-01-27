# Type Punning (casting between pointer types)

Casting between pointer types (or type punning) is often very poorly understood in C and this is mainly due to the fact that there is no method that is well defined, there are typically 3 methods; 2 of them are implementation defined (may/may not work dependent on the architecture you are building for) and the last is just straight up undefined in the majority of cases (but probably still will work).

CVM takes this mess and fixes it by just providing one way to cast reliably; `cast.<To, From>(from: From) -> From`.  This function is a core function that gets converted to one of the 3 methods dependent upon which cases we are talking about.  Furthermore it tags all pointers with original allocation size and alignment allowing it to detect at runtime if you are invalidating any C alignment rules (the majority reason why things break).  It also allows the following function if you want to reinterpret the bits i.e. `bitcast.<To, From>(from: *From) -> To`.

> We can avoid the implementation specific behaviour (due to trap representation) since we only support x86 and ARM architectures right now (mainly x86).

If any of the following rules are broken an error is raised by the interpreter with a stacktrace unless executed in `force-c-ub` in which case the behaviour is undefined.

## Important restrictions CVM places on the architecture/compiler

- We only support x86 and ARM right now
- Signed integers must be stored as 2's complement
- A linear memory model must be used (i.e. any pointer must be convertible to a unique integer value for it's address) this means that `intptr_t` and `uintptr_t` are well defined.
- POSIX conformance is also required

## Value casts/non pointer casts i.e. int to float

### Widening/Promotion

> i.e. `(long)2` or `(double)3.5f`

1. You can widen any signed integer to any larger signed integer type.
2. You can widen any unsigned integer to any larger unsigned integer type.
3. You can widen any floating point type to any larger floating point type.

### Narrowing/Demotion

> i.e. `(char)5` or `(float)2.3`

1. You can narrow any signed integer to any other smaller signed integer as long as the value resides in the limits of the new signed integer type
2. You can narrow any unsigned integer to any other smaller unsigned integer as long as the value resides in the limits of the new unsigned integer type
3. You can narrow any floating point to any other smaller floating point as long as the value resides in the limits of the new floating point type
  - If this is invalidated due to accuracy then it'll just round to the closest float
  - However if it is invalidated due to size (i.e. INF) then it'll raise the error

### Unsigned to Signed (vice versa) conversions

Note: Due to us only supporting x86 and ARM we can make presumptions about the structure of unsigned and signed integers (or more importantly signed integers being 2's complement).

1. You can cast any unsigned integer to any signed integer type given that the value of the unsigned integer is within the limits of the signed integer type.
2. You can cast any signed integer to any unsigned integer given that the value of the signed integer is within the limits of the unsigned integer type.

### Floating to Integer casts

1. You can cast any unsigned or signed integer to any floating point number as long as it can be represented in the floating point number
  - Note: If it can't be represented due to accuracy (i.e. 1 billion and 2 may be represented just as ~1 billion) then it isn't an error.
  - If it can't be represented because the int is too large (i.e. 64 bit integer in a 16 bit float) that is an error!
2. You can convert any floating point number to a signed integer as long as the value is within the limits of the signed integer
  - i.e. `INF` will definitely cause an error, so will `NAN` and any value greater than the maximum signed integer value
3. You can convert any floating point number to a unsigned integer as long as the value is within the limits of the unsigned integer
  - In this case the floating point number can't be negative as well.

## Pointer Casts

### Null pointers

1. Null pointers of any type can be converted to any other type.

The problem with this rule is that it's impossible to build code for in the majority of cases so instead we just will detect this at runtime and allow the cast even if it isn't allowed due to other reasons (i.e. alignment).

### Function pointer casts

1. You can cast from any function pointer `T *` to any other function pointer `U *` and back and the value will remain the same

Note: this doesn't meant that you can cast it to `U *` and the call it!  It just guarantees that there is no value conversion needed for casting between function pointers.

### Casting to `void *`

1. You can cast back and forwards through `void *` freely.

Though note that casting from `T *` to `void *` to `U *` is solely defined on the ability to cast from `T *` to `U *`.

### Alignment rules for all casts (except null and void * casts)

1. `T *` is aligned as long as it's location is divisible by it's aligned requirement i.e. `fn<T> is_aligned(val: *T) => val % alignof(T) == 0;`
2. `T *` cast to `U *` passes the alignment requirement as long as `alignof(T) % alignof(U) == 0`
  - This means that casting to `char`, `int8_t`, `uint8_t` is always well defined in terms of alignment (though casting to `int8_t` and any signed `char` is only defined if the signed cast is defined)
3. `U *` to `T *` passes the alignment requirement as long as the original object was aligned as if it was `T *` or an object of an identical alignment size.
  - This means you can do `T *` to `U *` then back to `T *`.  It also means that if you set the alignment of `U` to the alignment of `T` (via `alignas` or just that the `alignof(U) == alignof(T)`) it is also valid.

### Casting between integer pointer types

Since the specification of signed integers is well defined (because of our restrictions on architecture) and the fact that integers can't have a trap representation casting between integer pointer types is well defined as long as the alignment rules are satisfied.

Of course if you are doing a signed int pointer to unsigned int pointer cast (or reverse) then it is only valid if the cast between the integers is valid and the alignment is valid.

Else the alignment will fail.

### Casting between floating point pointer types

As long as the cast between the value types is valid (according to value casts between floating point numbers) then the cast between the pointers is valid if the alignment rules are satisified

### Casting between struct pointer types

1. You can cast any struct pointer to a pointer to the first member of that struct.
  - Exception would be if support for an auto packing directive was added in which in that case it wouldn't be defined.

> Note: you can't cast a struct pointer to anything else but the first member of that struct, you can't even cast a struct pointer to another struct pointer even if the contents are the same (due to potential padding differences).

### Casting between integer pointers and floating point pointer types

In C this is not well defined (UB in a lot of cases and implementation specific in others) and in CVM will report a casting compile time error.

The common use case of this is to cast the bits of a floating pointer number and so CVM allows this through `bitcast` i.e. `bitcast<uint32_t>(4.0f)`.

> You can give it a pointer if you want, the easiest way to take an address of a literal is using `new` i.e. `&new float { 4.0f }` though do note that you don't do `&new *float { 4.0f }` because that'll allocate a new object!

CVM allows you to bitcast any type to any other type as long as the alignment requirements are met and the sizes are the same, it however doesn't allow you to cast to a pointer type (it can take a pointer type though but that is just incase you have a large object you don't want to copy twice).

> Do note that this means that you can't bitcast an object to a pointer and then be able to modify the original through the pointer.  You'll have to do a bitcast, then change the value, then bitcast back and set the original type to the re-bitcasted value.

### Casting between pointer types and integers

You can cast between any pointer type and `uintptr_t` freely, the actual value inside `uintptr_t` is implementation defined but is guaranteed to be unique for that pointer value's lifetime.

Function pointers can also convert to `uintptr_t` due to POSIX requiring function pointer sizes to be equivalent to normal pointers.

Note: we don't support `intptr_t` because it's bizarre (it's bizarre to have negative addresses) if you wish to view the offset difference between pointers use `ptrdiff_t`.

> However you should note that `ptrdiff_t` is not well defined in the difference beteen the two pointers is very large.  You should only compare pointers that are within an array with the number of elements less than the maximum value of ptrdiff_t.

`uintptr_t` is valid for difference between pointers (and is preferred!!) if the result is always going to be positive, only use `ptrdiff_t` if you care about offsets within a small array, even then prefer to store the offset as an index variable in something like `ssize_t`.

CVM provides aliases for most of these types (except for `intptr_t` since that isn't supported);

- `size_t` => `usize`
- `ssize_t` => `isize`
- `uintptr_t` => `uintptr`
- `ptrdiff_t` => `iptrdiff`

> And before I get a billion issues opened about how this is not C compliant due to us not supporting `intptr_t` but supporting `uintptr_t`, you probably also want to open an issue around how we don't support raw union casts, don't support `iso646.h`, `setjmp.h`, trigraphs/digraphs and other unsafe and bizarre choices (`setjmp` isn't necessarily bizarre but we support it through coroutines safely!)...  Basically CVM is in no way guaranteed to be C compliant it however will always produce C complaint transiplation code and guarantees equivalent semantics to the produced transpiled code (noting of course that UB allows us to throw errors or do reasonable stuff).
