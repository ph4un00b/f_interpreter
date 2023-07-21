# estructura ✨
  - **grammars**: contiene poc's de PEG con la librería [pest](https://pest.rs/book/)
  - **lox_parser**: minimal parser for `1 - (2 * 3) < 4 == false`
    - ✅ let variables
    - ✅ loops
    - block scope
    - functions
  - **linked_list**: since block scoping is similar to a linked-list, I will take tour on this
    - following: https://rust-unofficial.github.io/too-many-lists/index.html
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

