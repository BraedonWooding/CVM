# MIPS Directives

For each directive I'll list which versions it's available in.

- The list is [here](https://www.cs.unibo.it/~solmi/teaching/arch_2002-2003/AssemblyLanguageProgDoc.pdf)

## Legend

You can separate multiple using `/` i.e. `m32/m64` signifies mips32 or mips64

- `*` signifies support across all
- `m32` signifies support from mips32
- `m64` signifies support from mips64
- `spim` signifies support from spim
- `mIV` signifies support from mips IV

## `.text` *

Place all data and instructions following this in the text segment of memory.

## `.data` *

Place all data and instructions following this in the data segment of memory.

> By default the data segment is non executable

## `.space N` *

Allocate 'n' uninitialised bytes of space in the current segment of memory.

## `.align N` *

Align next datum item to 2^N boundary.  Also aligns label if label was to be aligned to the datum.

Doing `.align 0` will turn off automatic alignment of datum items till next data segment (this includes alternative data segments i.e. `.kdata`, `.sdata`, ...)

By default the following boundaries are present

- `.half`, 

## `.comm id, expr` m32/m64/mIV

> All but SPIM

Unless `id` is defined elsewhere it becomes a common symbol globally at the initial point of `expr` bytes of storage.

If multiple `id`s are defined with the same name then only one symbol is defined but with the max value of `expr` and effectively it is overlaying all ontop of each other. (But they aren't separate storage wise).

## `$gp` commands m32/m64

These are only supported when running in PIC mode (only supported on m32/m64)

You can run in PIC mode by either forcing it in the code (using `.option pic2`) OR by using command line option `--option pic2`

> In non PIC mode all cploads are ignored, typically we like to recompile our ASM when changing modes (especially due to how cheap it is to pass / compile) so I don't recommend shipping the CVM bytecode for MIPS.  If you are going to ship CVM bytecode for MIPS then ship it with PIC enabled that way you can enable it in your code and have it work or disable it and still have it work (it uses define blocks).  Where as if you ship 

When generating PIC code in CVM bytecode we always need to wrap it in a define block i.e. `.cpload $t9` becomes the following MIPS

```mips
# in this case I'm just saying _gp_disp == 0 for simplicity
lui   $gp, 0
addui $gp, $gp, 0
addu  $gp, $gp, $t9
```

Which is the following in CVM if using PIC and proper function stepping using 32 bit mode

```asm
.if     def(PIC2)
mov     $gp, 0          : as imm
# uint is u32 on MIPS32 and u64 on MIPS64
add     $gp, $gp, 0     : as imm uint
add     $gp, $gp, $t9   : as uint
.endif
```

A good guide is [here](https://techpubs.jurassic.nl/manuals/0630/developer/Cplr_PTG/sgi_html/apa.html)

> mIV support may come eventually

Also note that if you want to use `bal` to call a subroutine that is PIC you need to designate an alternate entry point i.e.

```mips
# technically turning on/off reordering isn't needed since we never reorder
# but for completeness I'll include it
foo: .set     noreorder # callee
     .cpload  $t9
     .set     reorder
foo_1:                  # alternate entry
     ...
     jr       $ra
...
bar:                    # caller
     ...
     bal foo_1          # by-pass cp load
     ...
```

In generation we should keep to standards of MIPS which includes 

### `.cpadd reg`

Emits code that adds `_gp` to register `reg`.

> `_gp` acts as a global pointer that points into the middle of the 64K block of memory in the heap that holds constants / global variables, objects in this heap can be accessed using a single load/store

### `.cpload reg`

Loads `$gp` using `reg` and the constant `_gp_disp`

> `_gp_disp` designates the distance between the `lui` instruction and the context pointer.

```mips
lui $gp, _gp_disp
addui $gp, $gp, _gp_disp
addu $gp, $gp, reg
```

This must exist at the start of every subroutine that uses `$gp` with the exception of leaf procedures that don't access global variables, and procedures that are static. (non `.globl` or `.extern`)

You should always use `$t9` (`$25`) for `.cpload` since it is always used for indirect function calls and you should enforce no reorder mode.

This means that if you have an indirect jump unconditionally i.e. `jarl location` you must convert it to;

```mips
la $t9, location
# the previously talked about guide gets this wrong...
# maybe they using some assembler option that chooses correct jump?
# they used j $t9 instead of jr $t9
jr $t9
```

### `.cprestore offset`

The assembler will produce the following code immediately;

```mips
sw $gp, offset($sp)
```

And then later on after every single `jal` operation (BUT not `bal` operations) it'll emit the following;

```mips
lw $gp, offset($sp)
```

The programmer is responsible for reserving the space for the $gp pointer in the stack.

### `.gpword local-sym`

Used for PIC switch tables, and is similar to `.word` except it results in a 32 bit value that is the offset between `local-sym` and `$gp`.  `local-sym` must be local.

## Datums

All datum's

- \* `.byte e1 <e2, ..., eN>` followed by a comma separated list of expressions and truncates the to single bytes each, it then assembles the bytes in following consecutive positions.
  - You always need atleast one expression and can follow it by `: e2` if you only have one expression to signify that it should be repeated 'e2' times.

## Nops

These directives effectively act as nops

- `.alias r1 r2`, this is purely used to improve scheduling of instructions
  - We probably could support this in the future but currently no plans
- `.set noreorder`, `.set reorder` the compiler performs no reordering currently
  - It may in the future but currently these are ignored

## Todo

If detected these just show a warning... currently these require some work to implement

- `.aent`, `.end`, `.ent`,  => we need debugger support

## Compiler Only

If detected in user code will produce an error

- `.bgnb`, `.endb`, `.asm0`
