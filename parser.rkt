#lang brag
expres : /feed? expre
@expre : /SPACE? exprS /(SPACE|feed|edent)?
@exprS : exprF
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

func   : /FUNC exprF
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

apply1 : exprQ /SPACE expr1
applyQ : exprQ /SPACE kv0
       | (group|applyG) kv0
       | group e
applyO : expr0 op
apply0 : exprO dot
       | op (e|dot)
       | num id
applyG : (exprO|param|op) group

@e : num
   | string
   | id

num    : INTEGER | DECIMAL
int    : INTEGER
id     : ID
op     : OP
kv0    : @key (op|exprO)
kv1    : @key /SPACE exprF
kv2    : @key dent
key    : KEY (/SPACE? KEY)*
param  : PARAM PARAM?
dot    : /DOT (op|id) BIND?
string : /QUOTE /INDENT (STRING|interp|NEWLINE)* /DEDENT /UNQUOTE
       | /QUOTE         (STRING|interp)*                 /UNQUOTE
interp : INTERP (curly | dent)
@group : paren | curly | brakt
paren  : /LPAREN (@expre|@dent /feed|op) /RPAREN
curly  : /LCURLY (@expre|@dent /feed) /RCURLY
brakt  : /LBRAKT (@expre?|@dent /feed) /RBRAKT
dent   : /INDENT expre /DEDENT
edent  : /INDENT edent? /DEDENT
feed   : /NEWLINE+
