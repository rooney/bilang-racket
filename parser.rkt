#lang brag
expres : /feed? exprE
@exprE : /SPACE? expr3 /(SPACE|feed|edent)?
@expr3 : apply3
       | expr2
@expr2 : apply2
       | exprF
@exprF : func
       | applyF
       | applyM
       | applyK
       | commaK
       | exprK
@exprK : comma
       | comma0
       | commaO
       | commaQ
       | comma1
       | expr1
@expr1 : apply1
       | exprQ
@exprQ : applyQ
       | param
       | exprO
@exprO : applyO
       | expr0
@expr0 : apply0
       | applyG
       | group
       | dot
       | e

apply3 : expr2 /NEWLINE expr3
apply2 : (comma|comma0|commaO|commaG|commaQ|exprQ) (/SPACE apply2|dent)

func   : /FUNC expr2
applyF : (comma|comma0|commaO|commaG|commaQ|exprQ) /SPACE (applyF|func)
applyK : (comma|comma0|commaO|commaG|commaQ|exprQ) /SPACE (applyK|kv1)
       | (group|applyG) kv1
applyM : exprK /SPACE op
       | (exprK /SPACE)? op /SPACE exprF
       | (exprK /SPACE)? op (/SPACE exprF)? dent

@comma : exprK /COMMA
commaK : comma kv1
commaQ : comma (kv0|e)
       | (comma|comma0|commaO|commaG|commaQ) /SPACE kv0
comma1 : (comma|comma0|commaO|commaG|commaQ) /SPACE expr1
comma0 : (comma|comma0|commaO) dot
commaO : (comma|comma0) op
commaG : (comma|comma0 op?) group

apply1 : exprQ /SPACE (expr1|kv2)
applyQ : exprQ /SPACE kv0
       | (group|applyG) kv0
       | group e
applyO : expr0 op
apply0 : exprO dot
       | op (e|dot)
       | int id
applyG : (exprO|param|op) group

@e : id
   | string
   | num

num    : INTEGER | DECIMAL
int    : INTEGER
op     : OP
@id    : ID
@kv0   : @key (op|exprO)
@kv1   : @key /SPACE expr2
@kv2   : @key dent
key    : KEY (/SPACE? KEY)*
param  : PARAM PARAM?
dot    : /DOT (op|id) BIND?
string : /QUOTE /INDENT (STRING|interp|NEWLINE)* /DEDENT /UNQUOTE
       | /QUOTE         (STRING|interp)*                 /UNQUOTE
interp : INTERP (curly | dent)
@group : paren | curly | brakt
paren  : /LPAREN (@exprE|@dent /feed|op) /RPAREN
curly  : /LCURLY (@exprE|@dent /feed) /RCURLY
brakt  : /LBRAKT (@exprE?|@dent /feed) /RBRAKT
dent   : /INDENT exprE /DEDENT
edent  : /INDENT edent? /DEDENT
feed   : /NEWLINE+
