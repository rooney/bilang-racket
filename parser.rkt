#lang brag
return : /NEWLINE? expr4

@expr4 : expr3 (/SPACE|/NEWLINE|/INDENT/DEDENT)?
@expr3 : apply3
       | exprZ
@exprZ : kQz
       | exprK
@exprK : applyK1
       | applyK0
       | applyKO
       | Kc
       | expr1
@expr1 : apply1
       | exprQ
@exprQ : applyQ
       | symbol
       | exprO
@exprO : applyO
       | id
       | expr0
@expr0 : apply0
       | e

apply3  : exprZ /NEWLINE expr3
kQz     : (applyK0|applyKO|Kc|exprQ) /SPACE _Qz
@_Qz    : kQz | Qz
Qz      : (label|op) (/SPACE (exprZ|Qz)|dent)
@Kc     : exprK /COMMA
        | exprZ /NEWLINE /COMMA
applyK1 : (Kc|applyK0|applyKO) (/SPACE expr1|dent)
applyK0 : (Kc|applyK0) (dot|group)
applyKO : (Kc|applyK0) op
        | exprZ /NEWLINE op
apply1  : exprQ (/SPACE expr1|dent)
applyQ  : begin (label|exprQ|op)?
        | label (label|exprQ|op)
applyO  : expr0 op
        | expr0 dot
apply0  : exprO group
        | symbol group
        | OP e

@e : num
   | string
   | group

id : (num|OP)? ID
   | @id (op|dot)

op : OP
key : (OP|@id|@num)+
label : key COLON
symbol : (COLON key?)? COLON (op|id)?
dot : /DOT (OP|@id|group)
@num : INTEGER | DECIMAL
string : /QUOTE (STRING|group)* /UNQUOTE
       | /QUOTE /INDENT (STRING|group|NEWLINE)* /DEDENT /UNQUOTE

@begin : BPAREN | BBRACE | BBRACKET
group : /LPAREN expr4 /RPAREN
      | /LBRACE expr4 /RBRACE
      | /LBRACKET expr4 /RBRACKET
      | /BQUOTE indent
@dent : indent | undent
indent : /INDENT expr4 /DEDENT
@undent : /SPACE? /BACKSLASH @indent

