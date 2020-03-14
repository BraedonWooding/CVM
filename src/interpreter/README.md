# Interpreter

## Bytecode / IR Examples

Our bytecode is very similar to MIPS (it's just easier to generate) with a few exceptions

It also is intended to be very human writeable and does a huge amount of the annoying work for you (while also forcing you to do good assembly standards)

### Machine Independent Code

Instead of using explicit byte offsets we use variable byte offsets that are encoded upon loading into an interpreter.  We also have more explicit function frames.

You'll often see that this means that MIPS uses i32 (signed integer of 32 bits exactly) explicitly in a lot of places.

An example of a function is;

```mips
# In MIPS using PIC
foo:
    # our MIPS loader doesn't currently reorder so this isn't really relevant
    # but almost all MIPS assemblers do so for consistency I'll always include
    # them in examples where PIC is used.
    .set      noreorder
    .cpload   $t9
    .set      reorder
    subu      $sp, 4
    .frame    
    jr        $ra


```



```c
// == Alignment Example ==
// Also note: alignas(double) == align(alignof(double))
// Also on this system doubles are aligned at 4 bytes
// Foo(a: char[4], b: align(alignof(i64)) i32, c: double, d: i32)
FUNC Foo(4@1, 4@8, 8@4, 4@4):
  // ...
ENDF;

// == Recursive Example ==
// Fib(n: i32)
FUNC Fib(4@4):
  // the stack ptr is already setup to point here
  // so we just use 'registers' (note we don't have real registers in CVM
  // we just place register like things on the stack for variables / args)
  // $ refers to register
  // the '1' here is just a constant
  bgt $0 1 Fib_n_not_lt_1;  // $0 > 1
LABL Fib_n_lt_1:            // n < 1
  // the 'last' arg from last function is always the return value
  // so '-1', we put it in parentheses for style reasons
  // cpy allows us to write values into registers
  cpy $(-1) 1;
  ret;                      // return 1

  // we generate nice labels for simple if statements
  // else you'll just get Fib_if0, Fib_else0, ...
LABL Fib_n_gt_1:            // n >= 1
  mov $1 $0;                // $1 = $0
  // Note: we use 'as int imm' to signify the type of the sub
  //       in reality it gets encoded as sub_i32_imm but this looks nicer
  sub $1 1 as int imm;      // $1 = n - 1
  run Fib;                  // fib(n - 1)
  // move ret value
  mov $(-1) $2;
  // Note: the compiler may optimise this and see that n is immutable
  //       so it doesn't need to re-move things and sub 2 (just needs to sub 1
  //       more)
  mov $1 $0;                // $1 = $0
  sub $1 2 as int imm;      // $1 = n - 2
  run Fib;                  // fib(n - 2)
  add $(-1) $2;
  ret;                      // return fib(n - 1) + fib(n - 2)
ENDF;
```
