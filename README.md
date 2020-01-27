# CVM

> A virtual machine for C written in Rust

> Made by Braedon Wooding

## Why

Often when teaching I realised I need some way to show how C works behind the scenes and so I thought why not just simulate it.

So yeh this was a stupid idea...

But it may work and that's worth something.

> Rust was chosen because I thought 

## How does it work

There are typically 3 ways you can simulate something;

### Trace based simulation

This is often done when you want some kind of 'logging' or update simulation.

Look at LLV (my first project looking at this problem) for an idea of how this is done.  Effectively you write code using special tagged structures and then signal points for updates to occur.  This is pretty good but not perfect.

### Pure simluation

You just fully simulate the full instruction set on a virtual machine however often this means you lose a ton of source information and it gets hard to show visualisations.

It also is very much overkill.

### Hybrid (the method I chose)

You design a language almost identical to the original (in my case I changed very little) that importantly has the same semantics (sadly meaning the same flaws - i.e. can't return arrays).  This not only allows you to present semantically transpiled C (to allow debugging like views), but also alows you to add features that make visualisations easier (in my case we use a trait based system to allow printing out objects).

It is then compiled to a bytecode that is then executed!

#### Benefits of using this approach (custom language)

- No need to use special data structures you can just write data structures in PC and they'll show
- Code ends up being simpler allowing you to use higher level constructs
- Code is safer and will catch more of your errors
- Types can be inferred and all that fancy stuff as well as allow generic types (using templating)
- VM is pretty general and can be used for other languages
- Sandboxed to give you the benefits of address santiser + valgrind (or whatever)
  - Means it is also hopefully pretty secure
- It maps completely to C meaning code looks hopefully pretty idiomatic and won't require much work to pickup

#### Negatives

- It's bloaty, you have to pay for a whole compiler + VM setup
- It'll be slow (comparatively)
- Quite a few things won't be supported (random pointer arithmetic for one)
- All memory is allocated dynamically meaning measuring memory footprints is harder
  - Memory still has to be freed manually though since we need to generate equivalent C
  
## Samples

### Linked List (finding odds and separating into list)

```c
// everything you'll need is included by default
// currently PC can only be a single file

// We can use struct tags to give us free functions!!
// For example LinkedList comes with 'append'
// I won't use it for the sake of showing you some stuff
// but you can.
struct(LinkedList) Node<T> {
    data: T,
    // pointers go before
    next: *Node<T>
}

// Container really comes with nothing
// but it helps with printing.
struct(Container) List<T> {
    head: *Node<T>,
    tail: *Node<T>
}

// Variables use id : type = value
// You can use := to infer based on right type
// Inference isn't smart
// Functions can be defined inline but aren't closures
// (I may add closures in the future)
fn<T> find_odd(list: List<T>) -> List<T> {
    // everything default initialised by default
    // you can use '---' to force uninitialisation
    // C initialisation { } works as normal
    ret : List<T>;

    // dot for everything we'll figure out the right one
    cur := list.head;

    while cur {
        if cur.data % 2 != 0 {
            // found odd
            // you don't need '<>' if it can be seen from given
            // values (just standard unification)
            // to use it with functions you need a '.'
            appendList.<T>(&ret, cur.data);
        }
        cur = cur.next;
    }

    return ret;
}

fn<T> appendList(list: *List<T>, val: T) {
    // malloc is a generic now so it'll figure out the type
    // if you want arrays you can pass in a size
    // i.e. malloc(10);
    // you can use malloc<u8>(bytes); to allocate bytes
    newNode : *Node<T> = malloc();
    // initialiser lists casted to structures work
    *newNode = new Node<T> { .data = val };
    if !list.head {
        list.head = list.tail = newNode;
    } else {
        list.tail = list.tail.next = newNode;
    }
}

fn<T> freeList(list: *List<T>, val: T) {
    for (cur := list.head; cur;) {
        tmp := cur.next;
        free(cur)
        cur = tmp;
    }
}

// Arrays are very different since they need a defined length
// so always use pointers for the typical function array
// unless you want statically allocated array
// Pointers are smart and won't let you access out of bounds!
fn main(argc: int, argv: **char) -> int {
    // will be default initialised
    list : List(int);
    defer { freeList(&list); }

    // 'read' can help reading in multiple ints
    // we don't actually need the type here...
    // it's smart enough to type infer it from usage
    // but by specifying it we are just making the code clearer
    // read also works with strings
    while let i := read<int>(stdin) {
        appendList(list, i);
    }

    output := findOdd(list);
    defer { freeList(&output); }

    // we can use 'print' to print complex stuff
    // note: we can use '%' and it'll figure out the types
    // and replace it with the right printf string
    // When printing a list it'll print a simpler format of it
    // so that it fits on a single line
    // If you want the nice pretty print you can use pretty_print
    print("Original: %\nOdd Elements: %\n", list, output);

    return 0;
}
```

### A bunch of miscellaneous stuff

```c
// you can give no args (equivalent to (void) not to ())
// and you can give no return type for main (equivalent to int + return 0)
fn main() {
    // malloc is effectively calloc
    ints: *long = malloc(100, ints);

    // defer will run before function ends
    defer { free(ints); }

    // memcpy, memcmp, strcpy, strcmp all work...
    // don't use the 'safe' methods (strcpy and such in our implementation are safe)

    // arrays are always bound checked
    // note: they are still allocated on the heap
    //       but are shown as if statically allocated
    buffer: char[1024];
    buffer[0] = 0;

    // you can't assign to arrays because that would potentially arise confusion
    // since assigning to pointers doesn't cause a strcpy but assigning to arrays
    // would.

    // null is lower case (0 will also work)
    my_ptr := null;

    // we have bool types (will be translated to either stdbool.h or just int dependent
    // upon settings)
    tautology: bool = true;

    // casting is done through fn<Result, In> cast(in: In) -> Result
    // and is used like cast.<In, Result>(in)
    // how the casting occurs behind the scenes is a compiler thing
    integer := 4;

    // NOTE: we can infer the type of the integer...
    flt := cast.<float>(integer);

    // and we can utilise type inference to allow casting to become obvious
    one := 1;

    // i.e. the following wouldn't compile
    // half := one / 2.0;
    // but the following would (and it would convert the int to a float)
    half := cast(one) / 2.0;

    // this does mean that no implicit casts exist
    // OUTSIDE of widening integers/floats i.e.
    small: u8 = 2;
    large: u16 = 4000;
    // OK!
    sum := small + large;

    // but yes... no other conversions exist so no void * to pointers
    // no integers to floats... no signed to unsigned (or unsigned to signed)!
}

// sizeof has a weird definition it is effectively
// the '_' is because we don't actaully care about the arg
// it is just a way to infer T
// i.e. you can either do sizeof(my_arg) or sizeof<Type>()
///     (technically you can do both at the sametime i.e. sizeof<Type>(my_arg))
sizeof : fn<T> (_ : T = default) -> usize;
```

## Why is X Feature/Library not supported

Check out [theory crafting](theory_crafting/) to see features that are in their design phase and [proposals](proposals/) to see fully designed features.
