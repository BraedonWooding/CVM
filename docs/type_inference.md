# Type Inference Algorithm

The type inference algorithm can seem a bit obscure if you haven't covered the basics of programming languages.

The algorithm is loosely based on Hindley–Milner's type system and resulting type inference [here](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#An_inference_algorithm).  Though do note that we have a different type system (we don't have sum or product types that need to be type inferred they just represent new 'primative' like types).

You'll note that if you look at the algorithm and our code we don't have substitutions at all.  This is because instead every 'fresh'/'placeholder' type variable is given a unique identifier and then we carry a shared reference to the inherent type.  This means that any substitutions are committed instantly to a given placeholder type and effect all objects that use that type.  This does involve more state (so haskell'y people don't get too cranky) but really simplifies the algorithm (and makes it so much easier to write and to maintain as well as it's faster since the lack of laziness in Rust would make a lot more copies than in Haskell)

## Basics of type inference

Effectively type inference enacts to solve the following sort of equations;

```c
??? a = 2;
??? b = f("hey");
??? c = (&(*&(("hey")[2])))[0];
// so on...
```

To derive the types we'll utilise 2 forms of 'types'

### Concrete Types

These are types that are fully resolved of dependencies a few examples are;

```c
??? a = 2; // a is int
??? b = a; // a is concrete therefore b is concrete
```

> The resolution of these are extremely straightforward

### Placeholder Types

However the following type isn't concrete

```c
??? bar(??? b);

??? foo(??? a) {
  printf("Char: %c", bar(a));
}

??? bar(int b) {
  return b + '0';
}
```

The types of foo are dependent on bar which haven't yet been formed as a concrete type so we can't fully resolve foo until we resolve bar.

Now a naive solution may just delay resolution of foo till bar and while that would work in this case it would fail the recursive case (you can't delay resolution for a recursive function).  So we need to instead do something called placeholder types (in cvm we call them 'fresh' types just as a convention since they are generated freshly and are unique).

Effectively in this case we would create new types (we use integral ids but here I'll use the upper case alphabet);

```c
A bar(B b);

C foo(D a) {
  printf("Char: %c", bar(a));
}

E bar(int b) {
  return b + '0';
}
```

First off we have a few things that we can instantly note for example it should be clear that `B := int` and that `A := E`.  But we need an algorithmic way of deciding this... so we use something called unification!

## Unification

Unification is pretty straight forward here are the rules...

> Note: unification has the side effect of also type checking that no type constraints are invalidated (i.e. you don't pass a double to method requiring a struct) so we always run it even if both LHS and RHS are known to be concrete.

#### Case 1) Both are primative types i.e. 'int' or 'double' or a struct/union

In this case we just check if they refer to the same type name (note: typedefs ignored so if you do `typedef int myInt` then `myInt` would still be equal to `int` often languages provide a `newtype` for the sake of when you don't want this to happen).

If they don't match the same type then we can raise an error.

#### Case 2) Both are placeholder/fresh type 'variables'

Effectively this is like saying `A = B`, which just means that we choose one of the type variables and make all the types of that point to the other one.  In CVM we pick the first type as the one to point to the second for example in the code sample given `A := E` (meaning that all 'A's now are 'E's).

> Another way you can do this is to make a new type variable `C` such that `A := C` and `B := C` (avoids choice of promotion) often this is just obscure.  It doesn't matter which one you choose because you want all the type variables to go away anyways.

#### Case 3) One is a placeholder/fresh type variable and the other is just some other random type term

> Function types in functional languages only ever take one argument if you want to take multiple you have that function return a new function (and so on) for example if we want to represent `(int, float) -> int` we would do it as `int -> float -> int`.  This doesn't really matter and as you'll see CVM just regards function types as `(int, float) -> int` in it's type inference (because it's easier)

Basically this is like saying `B := int` in the code sample.  We just need to do a small check to make sure we don't get recursive types and that is that the type of the LHS doesn't depend on the type of the RHS.  This is called an 'occurs' check because all we do is check that the type variable doesn't occur in the other term at all!

> If it occurs then you have a type like `A := A -> B` which if we tried to expand would give us `A := A -> A -> B` infinitely.

It is a bit hard to formulate an infinite type in C but we can do it!

```c
// A -> int
int foo(??? g) {
  return 2;
}

int main(void) {
  // occurs check woudl fail
  foo(foo);
}
```

No the problem with `f(f)` is clearly it's an infinite type if we try to use `f` in any real capacity but more importantly for type inference we can't deduce A since the type of A depends on the function itself so we get `A := A -> int` which fails occurs check.

If it doesn't fail the occurs check then we can just set `A := RHS` as you would with 2 type variables (but there is only one to choose from).

#### Case 4) Pointer Types involving type variables

> This is a new case

This occurs when you have the following sort of types `a* U b*`.  The system is very simple we just unify the inner member i.e. `a := b` in this case.

> Note: if you have `a* := b` then you run into case 3 (the occurs check)

#### Case 5) Function Types

All we do is just unify each and every argument with the corresponding one we also unify return arguments.  Note that arguments may rely on each other but that shouldn't matter (the order of type inference for this algorithm is not important).

i.e. for `A -> B -> C` and `C -> A -> B` we would just unify each one, one by one...

- `A := C`
- `B := C` (note that we can carry across the above transformation here)
- `C := C` (same)

so we get `C -> C -> C` as expected.

Technically this also exists and holds for sum/product types (i.e. unions/structs) but CVM (and C) doesn't allow unification of structs.  Every struct is a unique type that has to be fully typed.

## Operators

Operators are just functions for the sake of this type inference algorithm for example addition of integers is just `int (int a, int b)` or `int -> int -> int`.

> We'll see later that we can generalise this

## What happens if we have any type variables left by the end

If we have any raw type variables when we are doing any step past the type inference stage (we could store a set of all type variables and remove them as we add proper concrete types or we could just check for it in future passes) then we have a problem.  Basically we have a function or a struct that should be explicitly polymorphic a classical case is...

```c
??? id(??? a) {
  return a;
}
```

The above type becomes `A -> A` which is fine but the problem is that `A` could be any type!  This is disallowed in CVM because it's not allowed in C.  The way around it is to designate it as polymorphic...

## Polymorphism

Polymorphism is a unique layer ontop of type inference.  It effectively states that you are going to all any type that implements a series of constraints.

For example the identity function (i.e. x = x) can be implemented polymorphically in CVM as...

```c
fn<T> id(a: T) -> T {
  return a;
}
```

> Operators are also polymorphic

Polymorphism can be thought of as a preprocessor.  Basically we end up doing 2 passes for each polymorphic function (or struct) the first one is just to set up polymorphic types and the second is to make sure that all constraints are met.

Polymorphism adds an extra unification rule.

#### Case 6) Polymorphism

If a given type is polymorphic then what we do depends on the other type...

- The other is a type variable => We already handle this in Case 3)
- The other is any type term => For example T foo = 2 this just means that we set `T := int` in this case (or rather we follow the case of type variables)
- The other is a polymorphic type variable => Same as type variable

For example if we do;

```c
fn<A> add(a: A, b: A) -> A {
  return a + b;
}
```

Then we would get that `A := T` where `T` represents the type parameter for the add operator `(+)`.  All operators have unique type parameters that basically allows us to confirm that indeed that operation is valid.

A better example would be...

```c
fn<T> castFlt(in: T) -> double {
  return cast(in);
}
```

In the above case we are calling `cast` (which is actually a polymorphic function).  A few important type relations is that `cast` has 2 type parameters `In` and `Out` in this case `Out := double` (clearly) and `In := T` this is the important bit
