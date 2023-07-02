# glossary

- prefix `++A`
- postfix ` A++`
- infix `A * B`
- mixfix `A ? B : C`
- level of precedence for instance  `js has (17 - 21 modern js) levels`
- precedence `aka weight aka just a number ╰(*°▽°*)╯`

# typical behaviors
- nud (right side)
- led (left side)
- on equal precedence, it defaults to left side `A + B + C => (A + B) + C`

# typical methods
- parse-next-expression
- consumer
- advance
- register
- get-precedence? or default to zero

# Parser minimal structure
  - tokens
  - iterator for tokens
  - hashmap of prefixes operators
  - hashmap of infixes operators
  - precedence table

# more
 - http://journal.stuffwithstuff.com/2011/02/13/extending-syntax-from-within-a-language/
 - pyhton impl: https://github.com/Mountagha/bantam/blob/main/parser.py
 - python grammar: https://effbot.org/zone/simple-top-down-parsing.htm
