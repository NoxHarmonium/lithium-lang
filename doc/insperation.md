# Inspiration

Notes about languages to draw inspiration and ideas from.

I will start with languages I know the least and work back to languages I'm familar with.

### Languages I'm Familiar With

- Ruby
- JavaScript
- Python
- C#
    -
- Swift
    - Still takes concepts from Objective C such as dynamic dispatch, late binding and extensible programming
    - Compiled
    - Null safety - Compiler checked null accesses
    - "Protocol orientated programming"
    - No pointers
    - Good syntactic sugar to clean up null checks, error handling and resource clean up (e.g. guard, let, defer)
    - ARC (Automatic reference counting)
    - Playground and REPL
    - Pattern matching in switches
- Lua
    - Lightweight and simple
    - Only 180 KB compiled so good for embedding
    - Array indexes start at 1 (!)
    - Made so that it starts simple but can be extended easily
    - Explicit lexical typing, dynamic by default
    - No arrays - Tables are used for everything
    - Only built in types: boolean, number, string and tables
    - Proper tail calls (similar to tail recursion)
    - Coroutines for light threading
- Java
    - JVM
    - Garbage collected
    - OO
    - Syntax influenced by C++
    - Statically typed
    - Parametric types through generics
- Kotlin
    - Statically typed
    - Inferred types
    - Good functional toolkit
    - Expressive language that can be used to make DSLs
    - Can be used as a procedural language if classes are omitted
    - Null safety - Compiler checked null accesses
    - Garbage collected (JVM)
- C
    - Static type system (weak)
    - Very low level
    - Inspired by ALGOL
    - Lexical scope and recursion (woo)
    - Simple
    - Manual memory management of heap
    - Basis of many languages
- C++
    - Imperative OO
    - Template meta language
    - Focus on performance
    - Close to hardware
    - RAII - can allocate heap objects like stack variables so they are destroyed when they go out of scope
    - Function overloading
    - Can cast with static or dynamic casting (compile time or runtime)
- Objective C
    - Message sending (not for concurrency), runtime dispatching, no type checking
    - Can send message to null without crash
    - Selector syntax allows expressive method calls
    - Two step constructor (alloc/init)
    - Protocols (another name for interfaces) to enable multiple inheritance
    - Categories (class extensions) that can be added to class at runtime (swizzling). Can access private members.
    - Message forwarding (good for things like stubbing objects for testing)
    - ARC (Automatic reference counting)
- Haxe
    - Compiles into other languages (AS3/Flash, JavaScript, Python, C++ PHP, Neko, Java...)
    - Strong type checking but can degrade to dynamic as many of the target languages are dynamic
    - Algebraic data types
    - Parametric polymorphism (Generics) with bounded quantification
    - Type inference
    - Support for structural and nominal typing (structural for anonymous types)
    - Very similar to AS3/Javascript but more advanced
    - Abstract types (not the usual OO abstract) which allow you to reuse types for specific purposes, like wrapping a float with a unit for unit conversions - Interesting

### Lanaguages I Like But Haven't Used Much

- OCaml
    - Parametric types
    - Tail recursion
    - Garbage collected
    - Functors (Parametric modules)
    - Pattern Matching
    - All the usual stuff good functional languages have
- Haskell
    - Strong static typing
    - Non-strict evaluation (lazy)
    - Based on the semantics of Miranda
    - Monads!
    - Pattern matching
    - List comprehension
    - Pure functional
- Clojure
    - Dialect of lisp
    - Runs on JVM, CLR (.NET) and JavaScript engines
    - Treats code as data (S-expressions are parsed into data structures before being compiled)
    - Immutable dynamic typing
    - [Identity concept](http://clojure.org/about/state) to model states over time - Interesting
    - Explicit not automatic tail recursion (due to JVM limitation)
    - Lazy sequences
    - Software transactional memory system.
- Erlang
    - Garbage collected
    - Actors based in preemptive multiplexed threads called processes
    - Runs in own VM
    - Dynamic Typing
    - Eager evaluation
    - Module hot swapping - Interesting
- F#
    - Multi-paradigm (functional, imperative and OO)
    - Roots in ML
    - Expression based, even try is composable with a static type
    - Eager evaluation
    - Discriminated union (like Haskell types)
    - Function composition using <<, >>
    - Continuations in imperative mode by using let!
    - Supports units of measure checking for numbers for compile time checking of conversions etc. - Interesting
    - Garbage collected with CLR (.NET)
- Go
    - Garbage collected
    - Structural Typing
    - Type inference
    - Goroutines (light weight threads - partially pre-emptable)
    - Interfaces rather than virtual inheritanc, also type embedding
    - Builds self dependant executables
    - Concurrency primitives - channels
    - Select statement, blocks waiting for multiple channels and the first one to receive is the first one to be processed, others are discarded
        - Good for things like receive data OR timeout
- Rust
    - Memory safe using compiler checks (borrowing system)
    - Concurrency primitives
    - No null or dangling pointers
    - Type system inspired by Haskell called traits, implemented by constraints on variable declaration
    - Higher-kinded polymorphism
    - Mix traits together instead of classes
    - Inferred type declarations using let
    - Macros for compile time checks such as the format string for printf
    - No garbage collection any more
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
- CLU
    - Pioneer in OOP
    - Enumerators
    - ...to be continued
- Agda
    - Theorem Prover
    - Interesting but no concepts that I want to draw from at this time
- Coq
    - Theorem Prover
    - Interesting but no concepts that I want to draw from at this time

### Lanaguages I Just Discovered And Need To Checkout and Categorise

- Ceylon
- Gosu
- Fantom
- FrTime
- Racket
- Eta
- Vala
- Opa
- Ur
- Ada
- Eiffel

## List of Type System Characteristics Off Wikipedia With Notes

[https://en.wikipedia.org/wiki/Type_system] Reference Link

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
