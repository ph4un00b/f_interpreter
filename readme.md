# estructura ✨
  - **grammars**: contiene poc's de PEG con la librería [pest](https://pest.rs/book/)
  - **lox_parser**: minimal parser for `1 - (2 * 3) < 4 == false`
    - ✅ let variables
    - ✅ loops
    - ✅ block scope
    - ✅ functions
    - ✅ semantic analysis aka resolver pass
    - ✅ closures
    - ✅ classes
    - ✅ inheritance
  - **linked_list**: since block scoping is similar to a linked-list, I will take tour on this
    - following: https://rust-unofficial.github.io/too-many-lists/index.html
    - at the end it seems we can use the great old rust vec!
    - learnings:
      - std:collection -> LinkedList
      - DoubleEndedIterator trait applied in vec or linked list
      - https://rust-lang.github.io/rfcs/2570-linked-list-cursors.html#rationale-and-alternatives
      - IterMut trait
# semantic analysis
  - Write a chunk of code that inspects the user’s program, finds every variable mentioned, and figures out which declaration each refers to.
  - after parsing and before interpreting, any work that doesn’t rely on state that’s only available at runtime can be done in this way.
  - Variable resolution touches each node once, so its performance is O(n) where n is the number of syntax tree nodes. More sophisticated analyses may have greater complexity, but most are carefully designed to be linear or not far from it. It’s an embarrassing faux pas if your compiler gets exponentially slower as the user’s program grows.
# tokens
  - stateful: literal value, line
# parser
  - ast creation
  - error handling (logging them or accumulating then displaying them)
    * synchronization
# statements
  - Functions return values.
  - Procedures cannot return values.
  - print statement evaluates an expression and displays the result to the user
    - BASIC and Python have dedicated print statements and they are real languages. Granted, Python did remove their print statement in 3.0 . . .
    - why it is bad?
# evaluation
  - walking recursively the ast tree (slow)
  - conversion into bytecode (fast)
    - like ruby >1.9
# data types
  - primitives: int, byte, short, char
  - references: compound data structures

