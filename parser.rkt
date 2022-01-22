#lang brag
return : /INDENT expr4 /DEDENT /NEWLINE?
       | /newb? expr4

@expr4 : expr3 (/SPACE|/NEWLINE|/INDENT/DEDENT)?
@expr3 : apply3
       | applyE
       | exprM
@exprM : applyM
       | expr2
@expr2 : apply2
       | exprK
@exprK : applyK
       | exprJ
@exprJ : applyJ1
       | applyJ0
       | applyJO
       | expr1
@expr1 : apply1
       | exprB
@exprB : applyB
       | symbol
       | exprO
@exprO : applyO
       | id
       | expr0
@expr0 : apply0
       | e

apply3  : expr2 /newb expr3
applyE  : applyM /NEWLINE expr3
applyM  : (exprK /SPACE)? op (/SPACE exprM)?
apply2  : exprK dent
applyK  : (xJ|applyJ0|applyJO|exprB) /SPACE (applyK|keyv1)
@xJ     : exprJ /COMMA
        | expr2 /newb COMMA
applyJ1 : expr2 /NEWLINE (keyv0|keyv1)
        | expr2 /NEWLINE op (dent|/SPACE expr1)?
        | (xJ|applyJ0|applyJO) (dent|/SPACE expr1)
applyJ0 : (xJ|applyJ0|applyJO) (dot|group)
applyJO : (xJ|applyJ0) op

apply1  : exprB (/SPACE keyv0)* /SPACE (keyv0|expr1)
applyB  : begin (symbol|exprO|op|dent)?
applyO  : expr0 op
        | expr0 dot
apply0  : exprO group
        | symbol group
        | OP e

@e : num
   | string
   | group

id : OP? num? ID
   | @id (op|dot)

op     : OP
@key   : (OP|@id|num)+
keyv0  : keys (symbol|exprO|op|dent)
keyv1  : keys /SPACE exprJ
@keys  : (keyc /SPACE?)* keyc
@keyc  : key COLON
symbol : (COLON key?)? COLON (op|id)?
dot    : /DOT (OP|@id|group)
@num   : INTEGER | DECIMAL
string : /QUOTE (STRING|group)* /UNQUOTE
       | /QUOTE /INDENT (STRING|group|NEWLINE)* /DEDENT /UNQUOTE

newb   : NEWLINE | BLANKLINE
@begin : BPAREN | BBRACE | BBRACKET
group  : /LPAREN expr4 /RPAREN
       | /LBRACE expr4 /RBRACE
       | /LBRACKET expr4 /RBRACKET
       | /BQUOTE dent
dent : /INDENT expr4 /DEDENT

