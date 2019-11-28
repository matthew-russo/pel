tokens:
  keywords:
    - type
    - dependent
    - on
    - contract
    - enum
    - object
    - fields
    - methods
    - functions
    - func
    - implement
    - match
    - for (loop and impl)
    - if
    - else
    - break 
    - continue ## do i want continue?
    - Self
    - self

  operators:
    - & (ref & bitwise)
    - * (ptr & mul)
    - /
    - +
    - -
    - !
    - &&
    - ||
    - <
    - >
    - >=
    - <=
    - :=
    - ==
    - !=
    - . (dereference)

  separators:
    - {
    - }
    - (
    - )
    - [
    - ]
    - ->
    - :
    - ::
    - ;
    - , 

  literal:
    booleanLiteral,
    charLiteral,
    stringLiteral,
    intLiteral,
    floatLiteral, 

  identifier:

precedence:
 separator
 operator,
 keyword,
 booleanLiteral,
 charLiteral,
 stringLiteral,
 intLiteral
 floatLiteral,
 identifier

