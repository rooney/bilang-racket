#lang brag
return : /NEWLINE? expr3

@exp32 : apply3 | applyZ | apply2 | pipe-2
@expr3 : apply3
       | exprZ (/SPACE | /NEWLINE | /INDENT /DEDENT)?
@exprZ : applyZ
       | apply2
       | pipe-2
       | exprL
@exprL : applyL
       | aptoOP|OP
       | exprK
@exprK : applyK
       | pipe-1
       | expr1
@expr1 : apply1
       | exprC
@exprC : applyC
       | pipe-0
       | exprO
@exprO : applyO
       | expr0 
@expr0 : apply0
       | label
       | alias
       | e

apply3 : exprZ /NEWLINE expr3
applyZ : packs /SPACE applyZ
       | keyop /SPACE apply2
       | keyop dent
apply2 : exprK dent
pipe-2 : piped dent
applyL : keyop /SPACE exprK
       | packs /SPACE applyL
aptoOP : packs /SPACE (aptoOP|OP)
pipe-1 : piped /SPACE exprL
applyK : pipe-0 /SPACE expr1
       | applyC /SPACE expr1
apply1 : exprO /SPACE expr1
 @packs : (keyop)| exprC
 @keyop : keyword|OP
@applyC : commaOP
        | comma
commaOP : comma OP
@comma : exp32 /NEWLINE /COMMA
       | exprL /NEWLINE? /COMMA
@piped : exprL /NEWLINE? /PIPE
       | exp32 /NEWLINE /PIPE
pipe-0 : piped (OP|exprO)
applyO : keyword exprO
       | expr0 OP
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
