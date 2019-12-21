# CVM

> A virtual machine for C written in Rust

> Made by Braedon Wooding

## Why??

Often when teaching I realised I need some way to show how C works behind the scenes and so I thought why not just simulate it.

So yeh this was a stupid idea...

But it may work and that's worth something.

> Rust was chosen because of TUI-RS being absolutely fabulous

## How does it work?

There are typically 3 ways you can simulate something;

### Trace based simulation

This is often done when you want some kind of 'logging' or update simulation.

Look at LLV (my first project looking at this problem) for an idea of how this is done.  Effectively you write code using special tagged structures and then signal points for updates to occur.  This is pretty good but not perfect.

### Pure simluation

Just a huge pain and often useless too hard to show parts of the stack that are useful...

### Hybrid

This uses a simple stack based virtual machine (in no way is it intended to be fast) where you write 'Psuedo-C' (which I've named PC) and it'll compile it to equivalent C like code and show you the code alongside the current representation of the stack (stack frames) as well as showing any data structures you are using.

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

## Features coming soon

> These features are still in the specification/idea stage

### Enums

```c
// enums work as expect
enum Animals {
    DOG, // will be 0
    CAT, // will be 1
    BAT = 9, // override
    WOLF, // will be 10
}

fn print_animal(animal: Animal) {
    switch (animal) {
        case DOG: case WOLF: {
            printf("Woof\n");
        }
        // ... so on
    }
}

// NOTE: enums are also utilised in unions (as shown in Union section)
```

### Unions

```c
// unions are actually implemented as algebraic data types
// under the hood.  Another term for them is 'tagged unions'
// i.e. if you put an int inside a union you CAN'T take a float out!
union Data {
    // we support rust styled types
    // i.e. i32 == int32_t and usize == size_t
    // NOTE: int == int_fast32_t and long = int_fast64_t and so on...
    // (for uint and ulong and short/ushort)
    INT = (integer: i32),
    FLT = (decimal: f32),
    STR = (string: *char, len: usize) // NOTE: char is always unsigned
}

fn print_data(data: Data) {
    switch (data) {
        case INT: {
            printf("%d\n", data.integer);
        }
        case FLT: {
            printf("%f\n", data.decimal);
        }
        case STR: {
            printf("%.*s\n", data.len, data.string);
        }
    }
}

fn main() {
    // you construct them through this way
    print_data(new Data { .INT = { 5 } }); // '5'
}
```

Downsides currently are that they don't really have a lot of interaction with generics and no pattern matching makes them bit of a pain in cases...

You can force a change of type through:

```c
union Binary32Float {
    BINARY = (rep: i32),
    FLOAT  = (val: f32)
}

fn fltToData(flt: f32) => new Binary32Float { .FLOAT = { flt } };
fn printRep(data: Binary32Float) {
    // all unions have a '_type' field
    // and we can change the type explicitly this way
    data._type = BINARY;
    print("{}", data.rep);
}
```

There is a bit of compiler linting making you have to check the type before you are gonna use it (unless you explicitly set it).  If you try to print it out it'll just print out the type!
