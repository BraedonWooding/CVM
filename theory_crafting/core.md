# Theory Crafting - Core Additions

Effectively this introduces most of the common defaulted functions into a new module 'core'

## Allocator

### Type

The allocator type is more heavily detailed in the [allocator](allocator.md) theory crafting section.

For the sake of clarity the structure is repeated here;

```rust
module core;

/*
    When we get generic function pointers this gets cleaned up a bit (currently you can use generics in function pointers but can't define a generic in a function pointer)

    i.e. qsort works as fn<T> qsort(array: T[..], cmp: fn (a: *const T, b: *const T) -> int)
         but this is because the function pointer doesn't define any new generics!
 */
struct Allocator {
    // - To get around the function pointer fiesta we also pass in the alignment
    //   of the type (no allocator needs more than the alignment + pointer)
    //   and the size of the type (not the size of the allocation though!!)
    // - if you really want to inspect the type then you could also wait for the
    //   new core.reflect module.

    // - alignment must be <= alignof(maxalign_t)
    //   and must match the original alignment allocated
    fn_free : fn (arg: *void, type_size: u8, alignment: u8),

    fn_alloc : fn (size: usize, alignment: u8) -> *void,

    // the old alignment == new alignment after realloc
    // also old type_size == new type_size
    fn_realloc : fn (original: *void, type_size: usize, alignment: u8,
                     new_size: usize) -> *void,
}
```

### Global allocator

In the core module an external global allocator is made.

i.e.

```rust
// extern const behaves semi weirdly
// you can't modify it at all (i.e. you can't do CVM_GLOBAL_ALLOCATOR = x)
// but you can override the definition if you define it exactly the same
extern const CVM_GLOBAL_ALLOCATOR: const Allocator = CVM_C_ALLOCATOR;
```

You can modify this global allocator by just defining this...

```rust
// some_other_file.c

// choose some allocator (could be your own allocator for example)
// void allocator just has empty bodies and returns null for all allocations
extern const CVM_GLOBAL_ALLOCATOR: const Allocator = CVM_VOID_ALLOCATOR;
```

### Impact on C transpilation

Global allocators are a compile time feature that is removed inline

## `new`

`new` will not change since it's a language mechanic and not a function.

However `alloc` which is a child function of `new` that actually allocates the memory (note: `new` doesn't always allocate but when it does it'll use `alloc`).

Alloc maps to the global allocator (unless one is passed to new)

## `free`

Will become a core function (maps to default allocator.free which is C free).

TODO (there are a lot more...)
