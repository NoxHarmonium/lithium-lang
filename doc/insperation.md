# Inspiration

Notes about languages to draw inspiration and ideas from.

I will start with languages I know the least and work back to languages I'm familar with.

### Languages I'm Familiar With

- Ruby
- JavaScript
- Python
- C#
- Swift
- Lua
- Java
- Kotlin
- C
- C++
- Objective C
- Haxe

### Lanaguages I Like But Haven't Used Much

- OCaml
    - Parametric types
    - Tail recursion
    - Garbage collected
    - Functors (Parametric modules)
    - Pattern Matching
    - All the usual stuff good functional languages have
- Haskell
    -
- Clojure
- Erlang
- F#
- Go
- Rust
    -
- D
    - Array slicing
    - Lazy evaluation
    - Compile time function execution
    - Garbage collected
    - Design by contract (preconditions, postconditions, invariants) - Interesting!
    - Actor model concurrency
- Scala
    - All functions can be infix
    - Functions can have symbols (e.g. + is just a standard function)
    - Methods with no paren brackets
    - Colon operator can swap receiver and argument (2)f instead of f(2)
    - Can use curly braces instead of parens in function calls e.g. Vector.fill(4) { math.random }
    - For-expressions - Nice!
    - Delimited continuations - Need to look into this further
    - All the usual stuff good functional languages have
    - JVM (Garbage collected)
- Julia
    - Easy interop with C/Fortran without glue code
    - Multiple dispatch (overloading functions)
    - "All concrete types are final and may only have abstract types as their supertypes"
    - Parametric types
    - Nominative types
    - All objects are true object in a fully connected type graph. (No primitives)
    - Unicode Support
    - Garbage Collected

### Obscure (to me) Languages That Have Similar Ideas To Me That I Only Discovered Doing Research For This Language

- E
    - Constraints on function in/out for auditing
    - Every object gets its own thread, like Erlang but full threads not lightweight processes
- Occam
    - Defines parallel and sequential operations explicitly
    - Communication through "channels" using ! (output) and ? (input)
    - Guarded commands - Accept input until condition is met and then output (Interesting)
- Forth
    - Can modify program during execution (true reflection)
    - Concatenative programming - Interesting
    - Stack based
    - Very simplistic but very extensible
- Nim
    - Message passing
    - Algebraic data types
    - Simple syntax
    - Garbage collected
- Clean
    - Similar to Haskell
    - "Computation is based on graph rewriting and reduction. This, combined with compilation to native code, makes Clean programs run relatively fast, even with high abstraction." - Interesting!
- Curry
    - Based on Haskell
    -
- Agda
    - Theorem Prover
    - Interesting but no concepts that I want to draw from at this time
- Coq
    - Theorem Prover
    - Interesting but no concepts that I want to draw from at this time



## List of Type System Characteristics Off Wikipedia With Notes

(Link)[https://en.wikipedia.org/wiki/Type_system]

#### Type safety

- Strictly safe typing to pick up errors at compile time

#### Dynamic vs static type-checking

- Only static type checking to pick up errors at compile time.
- Need to be able to look up types at runtime to do things like parse JSON though

#### Inferred vs. Manifest

- Ideally inferred typing would be good but may be complex to implement. Manifest typing would not be the worst thing in the world, at least to start with.

#### Nominal vs. Structural

- I think Nominal may be better to better define types, there may be types with the same structure but different uses.
- Need to further investigate the benefits of structural typing.

#### Dependent typing

- I like the idea of being able to define a function that for example only takes positive numbers.
- This could feed into an idea of functions only acting upon a value if the value meets certain conditions, otherwise it is passed through
    - E.g. def something(x: SomeObject:{someProperty where someOtherProperty == true}) ...

#### Duck typing

- I don't want to use duck typing in this language. I want it to be strict static typing.

#### Gradual typing

- I don't want to add a dynamic typing component to the language so it will not have gradual typing

#### Flow-sensitive typing

- Flow typing relies on reflection which is off the table for now. It is more of a dynamic language concept

#### Latent typing

- More of a dynamic typing concept

#### Substructural typing

- Interesting concept. I see rust in the list of languages that support linear or affine types so it is worth looking in to.

#### Uniqueness typing

- Very important definitely look into this. Uniqueness enforced by message passing

#### Strong and weak typing



## Memory Management

- Scope based like C++ smart pointers (reference counting)