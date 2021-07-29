#lang brag
return : /NEWLINE? expr3

@expr3 : apply3
       | exprZ (/NEWLINE | /INDENT /DEDENT)?
@exprZ : applyZ
       | apply2
       | exprL
@exprL : applyL
       | OP
       | exprK
@exprK : applyK
       | commaX
       | expr1
@expr1 : apply1
       | exprO
@exprO : applyO
       | keyword
       | expr0 
@expr0 : apply0
       | label
       | alias
       | e

apply3 : exprZ /NEWLINE expr3
applyZ : (commaX|exprO|OP) /SPACE applyZ
       | OP /SPACE apply2
       | OP dent
apply2 : exprK dent
applyL : OP /SPACE exprK
       | (commaX|exprO|OP) /SPACE (applyL|OP)
applyK : commaX /SPACE expr1
@commaX : commaOP
        | comma
commaOP : comma OP
@comma : (apply2|applyZ|apply3) /NEWLINE /COMMA
       | exprL /NEWLINE? /COMMA
apply1 : exprO /SPACE expr1
applyO : expr0 OP
       | keyword exprO
apply0 : expr0 e
       | OP e

@e : INTEGER | DECIMAL
   | STRING 
   | ID
   | dot
   | group | PAREN | BRACE | BRACKET | undent

@subexpr : expr3
         | dent
@dent : /INDENT expr3 /DEDENT
@undent : /BACKSLASH dent

group : /LPAREN subexpr? /RPAREN
      | /LBRACE subexpr? /RBRACE
      | /LBRACKET subexpr? /RBRACKET

alias : label label+
@name : OP? ID OP?
label : COLON (OP|name)?
keyword : (OP|name) COLON
dot : /DOT name
