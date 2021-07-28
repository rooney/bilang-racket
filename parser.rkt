#lang brag
return : /NEWLINE? expr3

@expr3 : apply3
       | exprZ (/NEWLINE | /INDENT /DEDENT)?
@exprZ : applyZ
       | apply2
       | OP
       | expr1
@expr1 : apply1
       | exprO
@exprO : applyO
       | keyword
       | expr0 
@expr0 : apply0
       | e

@e : INTEGER | DECIMAL
   | STRING 
   | label | ID
   | group | PAREN | BRACE | BRACKET | undent
   | comma | dot

apply3 : exprZ /NEWLINE expr3
applyZ : exprO /SPACE (applyZ|OP)
       | OP /SPACE exprZ
       | OP dent
apply2 : expr1 dent
apply1 : exprO /SPACE expr1
applyO : keyword exprO
       | expr0 OP
apply0 : expr0 e
       | OP e

@comma : expr1 /COMMA
       | expr3 /NEWLINE /COMMA
@subexpr : expr3
         | dent /NEWLINE 
@dent : /INDENT expr3 /DEDENT
@undent : /BACKSLASH dent

group : /LPAREN subexpr? /RPAREN
      | /LBRACE subexpr? /RBRACE
      | /LBRACKET subexpr? /RBRACKET

@name : OP? ID OP?
keyword : (OP|name) COLON
label : COLON name?
dot : /DOT name
