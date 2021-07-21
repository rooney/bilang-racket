#lang brag

return : /NEWLINE? expr /NEWLINE?

@expr : expr1
      | apply2

@expr1 : term 
       | apply
       | apply1

apply : term /SPACE (expr|apply)

apply1 : expr /INDENT expr /DEDENT

apply2 : expr1 /NEWLINE (expr|apply2)

@term : INTEGER
      | DECIMAL
      | STRING
      | ID
      | paren | bracket | brace
      | apply0

apply0 : term term

paren : /LPAREN expr /RPAREN
bracket : /LBRACKET expr /RBRACKET
brace : /LCURLY expr /RCURLY
