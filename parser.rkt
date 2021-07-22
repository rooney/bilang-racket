#lang brag

return : /NEWLINE? expr

@expr : expr1 /NEWLINE?
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
      | /BACKSLASH /INDENT expr /DEDENT
bracket : /LBRACKET expr /RBRACKET
brace : /LCURLY expr /RCURLY 
      | /INDENT expr /DEDENT
