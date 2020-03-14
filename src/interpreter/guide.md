# Guide for CVM Bytecode

An informal mostly example driven guide.

## Registers / Memory

CVM Bytecode doesn't utilise typical register styles instead you utilise the stack to store your registers.  However this often doesn't fit for simulating MIPS / x86 so we provide a few ways of accessing memory.

```mips
# most of the time you'll just add the following and it'll automatically
# do the following work (+ a few other things)
# .style(cvm_simple)

.registers
  # you can put `.load MIPS32` to load a MIPS32 set of registers
  # and similar for the rest (MIPS64, MIPS IV, SPIM)
  # in this case we aren't simulating anything so we don't need register
  # definitions bar a few basic ones

  # .size will give you the length in bytes of a segment
  # and if used in a register mapping acts as a resolvable property
  .stack_pointer .reverse usize $sp = .size(.stack)
  .frame_pointer usize $fp = .cur(.stack)
  # you can also do stuff like $ra = $31 = ...
  # and you can use .init to give initial value (note it doesn't resolve to
  # anything since init is more like a function to call upon startup)
  .return_address usize $ra
  # you can have multiple return values
  # force_sequential forces them such that &($v0) + 1 == &($v2)
  .return_value .force_sequential i32 $v0, $v1
  # $ refers to a value, & refers to the address,
  # * refers to a dereference of addr
  # overlay let's you put a register over another (can span multiple just can't
  # span over empty space)
  .return_value .overlay(&v0) i64 $v
  .temporary usize $t0, $t1, $t2, $t3, $t4, $t5, $t6, $t7, $t8, $t9

.text
# actual entry point
# note: that we don't care about 'pointers' just the sizes
fn __start__(i32 $argc, usize $argv):
  # load initial value
  move    $ra, label:(__start__, usize)
  # frame sets up a frame
  frame   __main__
  # pass args through (pushes onto stack if past a certain arg)
  move    $0, argc
  move    $1, argv
  # __main__ is a constant that the user can define but by default it is just
  # main...
  # call does a lot of the work for you
  call    __main__
  frame   SYSCALL_EXIT
  move    $0, $v0
  syscall SYSCALL_EXIT
  # should never reach here
  trap    TRAP_UNREACHABLE
endfn

# user code
.data
  fmt: .asciiz "%d^%d is %d\n"

.text

.entry
fn main(i32 $argc, usize $argv):
  # for each movepair (i, value) in argv print out pow ^ i
  .var    $i, i32
  # argv includes the name of executable
  # imm: is a constant
  move    $i, imm:(1, i32)
while:
  bge     end, $i, $argc
  # t0-t9 are temporary (useful)
  # in this case we don't need to use this but I am to show it off
  add     $t0, $i, $argv
  frame   pow
  # * is dereference since we don't know the type we have to specify
  # like *:($t0, i32) unless we want i32 which is the default
  # (like how usize is default for &)
  # there is a quicker way to do this style as we'll see later
  move    $0, *t0
  move    $1, $i
  call    pow

  # we could actually also push the frame of printf on before we
  # called pow and save this extra addition and deref...
  frame   printf
  move    $0, label:(fmt, usize)

  # a trick here that will work is add &1, $i, $argv
  # this will 'set the $1 to point to ($i + $argv)' in reality it won't move
  # the address of $1 and instead just move the deref'd value
  # effectively we get;
  # - $1, $i, $argv => $1 = $i + $argv (i.e. $1 is a value)
  # - &1, $i, $argv => *1 = $i + $argv (i.e. deref and set to address)
  # - *1, $i, $argv => *1 = *($i + $argv) (i.e. deref both set to value)
  # - !1, $i, $argv => $1 = *($i + $argv) (i.e. deref just right hand side)
  # ^^ is used as indexing in some languages (haskell)
  add     !1, $i, $argv
  move    $2, $i
  move    $3, $v0
  call    printf

  # add will specialise to a sub add type dependent on types of both args
  # could also use inc $i here
  add     $i, $i, imm:(1, i32)
  # unconditional
  jmp     while
end:
  move    $v0, imm:(0, i32)
  # ret always uses $ra (or .return_address) to go to next
  ret
endfn

fn pow(i32 $x, i32 $n):
  # push room for $acc onto stack which is size i32
  .var    $acc, i32
  # move 1 into acc, imm stands for immediate
  move    $acc, imm:(1, i32)
# labels are hygenic (i.e. you can't jump across functions)
# if you want to expose a label there are a few ways;
# 1) Define it outside of a function
# 2) Use `.globl` or `.extern` signifiers (globl defines it in translation unit
     extern defines it in all translation units)
while:
  bge     end, $

end:
  # could just do everything in $v0 and that's also fine...
  move    $v0, $acc
```

## Directives

## Instructions
