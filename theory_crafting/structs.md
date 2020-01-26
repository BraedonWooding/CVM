# Theory Crafting - Structs

There are a series of improvements that could be made to structs, in order to keep CVM initially simple these are kept out; but I will look to add them upon initial release.

## Struct Members

```c
struct Set<T> {
    // T[..] is an array of unknown length
    // so it carries a length with it
    // This is actually also a new 'feature'
    // under core
    inner: T[..]

    // technically this doesn't conflict with
    // new <> since this is a struct member
    // so it's name is really 'set_new'
    // but keep in mind if you want to use
    // it you'll have to write
    // Set.new() regardless of where you are
    fn new(initial_size: usize = 0) {
        new <Set<T>> {
            // --- to uninitialise values
            inner: new T[initial_size] { --- }
        }
    }

    // Note: this will compile as
    //       array: *T, len: usize
    fn from_array(array: T[..]) {

    }
}

```

Converts to


