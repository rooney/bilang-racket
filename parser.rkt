#lang brag
return : /feed? expr4
       | /INDENT expr4 /DEDENT /feed?

@expr4 : expr3 (/SPACE|/feed|/INDENT/DEDENT)?
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
       | applyJO
       | applyJ
       | expr1
@expr1 : apply1
       | exprB
@exprB : applyB
       | exprD
@exprD : id
       | atom
       | exprO
@exprO : applyO
       | expr0
@expr0 : apply0
       | e

apply3  : apply2 expr3
        | exprK /feed expr3
applyE  : @applyM /feed expr3
applyM  : ( begin?        nuke /SPACE 
          | exprK /SPACE (nuke /SPACE)?
          )? op (/SPACE exprM|dent)?
apply2  : exprK dent
applyK  : (@applyJ|applyJO|exprB) /SPACE (applyK|kv1)
        | exprM /feed kv1
applyJ  : exprM (/feed|/SPACE) /COMMA
        | exprJ /COMMA
applyJO : @applyJ (exprD|op|kv0)
applyJ1 : exprM /feed kv0 (/SPACE kv0)* (/SPACE expr1)?
        | (@applyJ|applyJO)  (/SPACE kv0)* /SPACE (kv0|expr1)
apply1  : exprB              (/SPACE kv0)* /SPACE (kv0|expr1)
applyB  : begin (exprD|op)?
applyO  : expr0 op
apply0  : exprO ion
        | exprD group
        | op e

@e : num
   | string
   | group
   | ion

id     : op? num? ID op? ion*
op     : OP
kv0    : @nuke (exprD|dent)
kv1    : @nuke /SPACE exprJ
nuke   : (part COLON /SPACE?)* part COLON
atom   : (COLON part?)? COLON (@id|op)?
ion    : /DOT (@id|op)
@part  : (OP|ID|num)+
@num   : INTEGER | DECIMAL
string : /QUOTE (QUOTE|STRING|group|UNQUOTE)* /UNQUOTE
       | /QUOTE /INDENT (STRING|group|NEWLINE)* /DEDENT /UNQUOTE

@begin : PAREN | BRACE | BRACKET
group  : BQUOTE dent
       | /LPAREN expr4 /RPAREN
       | /LBRACE expr4 /RBRACE
       | /LBRACK expr4 /RBRACK
dent   : /INDENT expr4 /DEDENT
feed   : /NEWLINE+
