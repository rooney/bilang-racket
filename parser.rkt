#lang brag
return : /NEWLINE? expr3

@expr3 : apply3
       | exprZ (/SPACE | /NEWLINE | /INDENT /DEDENT)?
@exprZ : applyZ
       | apply2
       | pipe2Z
       | exprL
@exprL : applyL
       | extOP
@extOP : apptOP|OP
       | exp1p
@exp1p : pipe1L
       | exprK
@exprK : applyK
       | expr1
@expr1 : apply1
       | commaX
       | exprO
@exprO : applyO
       | expr0 
@expr0 : apply0
       | label
       | alias
       | e

apply3 : exprZ /NEWLINE expr3
applyZ : pack /SPACE applyZ
       | OP /SPACE apply2
       | OP dent
apply2 : exprK dent
pipe2Z : exp1p /PIPE /SPACE (apply2|applyZ)
applyL : OP /SPACE exp1p
       | pack /SPACE applyL
apptOP : pack /SPACE (apptOP|OP)
pipe1L : extOP /PIPE /SPACE exprL
applyK : commaX /SPACE expr1
apply1 : exprO /SPACE expr1
 @pack : exprO|commaX | (OP)
@commaX : commaOP
        | comma
commaOP : comma OP
@comma : (apply2|applyZ|apply3) /NEWLINE /COMMA
       | exprL /NEWLINE? /COMMA
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
