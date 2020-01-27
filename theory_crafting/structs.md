# Theory Crafting - Structs

There are a series of improvements that could be made to structs, in order to keep CVM initially simple these are kept out; but I will look to add them upon initial release.

## Struct Member Functions

```rust
// using modules version 1
use std.io;
use std.lib;
use std.assert;

// normally you would use like a hashset
// or maybe a b-tree, but I'm just gonna use a vector
// like set for simplicity.
struct Set<T> {
    // T[..] is an array of unknown length
    // so it carries a length with it
    // This is actually also a new 'feature'
    // under core
    inner: T[..],
    len: usize

    // technically this doesn't conflict with
    // new since this is a struct member
    // so it's name is really 'set_new'
    // but keep in mind if you want to use
    // it you'll have to write
    // Set.new() regardless of where you are
    fn new(initial_size: usize = 0) -> Set<T> {
        return new Set<T> {
            // --- to uninitialise values
            inner: new T[initial_size] { --- },
            len: 0
        };
    }

    // self pointers must be const
    // (note: this doesn't make the object const that would be *const self
    //        it just makes it so that you can't mutate the pointer)
    fn add(const *self, elem: T) {
        if !self.contains(elem) {
            if self.len >= self.inner.len {
                // initial size can't be 0 (else 0 * 2 == 0)
                if self.inner.len == 0 { self.inner.len == 1; }

                // reached capacity
                // resize is a method on T[..] that reallocs
                // resize in CVM typically means that it'll abort with a nice
                // error in the case of failure
                self.inner = self.inner.resize(self.inner.len * 2);
            }
            self.inner[self.len] = elem;
            self.len += 1;
        }
    }

    fn contains(const *self, find: T) -> bool {
        // using slices (new idea) and for 'in' it becomes
        // for elem in self.inner[0..self.len]
        for i = 0; i < self.len; i += 1 {
            if self.inner[i] == find {
                return true;
            }
        }
        return false;
    }
}

fn main(void) -> int {
    set := Set.new();
    set.add(5);
    set.add(10);
    set.add(2);
    assert(set.contains(5));
    assert(!set.contains(20));
}
```

Converts to

```c
#include <stdbool.h>
#include <assert.h>
#include <stdlib.h>

// '$' is a valid identifier on most compilers and is very commonly unused
// so we use it by default to specialise all our structs
// you can also change it using --specialise-char="_"
struct Set$int {
    // note that if you had declared a 'inner_len' member
    // it would have failed compilation due to how T[..] is defined
    // (it's a special always expanding struct regardless of where it is
    //  -- currently not userland available but I'm sure that'll change)
    int *inner;
    size_t inner_len;
    size_t len;
}

Set$int set$int_new(size_t initial_size) {
    return (Set$int) {
        .inner = malloc(initial_size * sizeof(int)),
        .inner_len = initial_size,
        .len = 0,
    }
}

void set$int_add(Set$int *self, int elem) {
    if (!set$int_contains(self, elem)) {
        if (self.len >= self.inner_len) {
            if (self.inner_len == 0) {
                self.inner_len = 1;
            }
            // inlined from resize
            void *cvm_tmp = realloc(self.inner, self.inner_len * 2);
            if (!cmp_tmp) {
                free(self.inner);
                abort();
            }
            self.inner = cvm_tmp;
        }
        self.inner[self.len] = elem;
        self.len += 1;
    }
}

bool set$contains(Set$int *self, int elem) {
    for (int i = 0; i < self.len; i++) {
        if (self.inner[i] == elem) {
            return true;
        }
    }
    return false;
}

int main(void) {
    set := Set$int_new();
    Set$int_add(&set, 5);
    Set$int_add(&set, 10);
    Set$int_add(&set, 2);
    assert($Set_int_contains(&set, 5));
    assert(!$Set_int_contains(&set, 20));
}
```
