#lang brag
expres : /feed? expre
@expre : exprK /(feed|void|SPACE)*
@exprK : exprJ
@exprJ : applyJ
       | expr1
@expr1 : apply1
       | expr0
@expr0 : e0
       | dot
       | group
       | applyG
       | apply0

applyJ  : expr1 j+
j       : /COMMA (expr0|dot+|dot* op) (/SPACE expr1)?
apply1  : expr0 (/SPACE kv0)* /SPACE (kv0|expr1)
apply0  : expr0 op
        | (expr0|op) dot
        | (group|op|applyG) e0
        | num id
applyG  : (expr0|op) group

@e0 : e0dot
    | num
    | id
    | string

func   : param+ brace
e0dot  : e0 dot
num    : INTEGER | DECIMAL
int    : INTEGER
id     : ID
op     : OP
kv0    : @arg (expr0|op)
kv1    : @arg /SPACE expr1
kv2    : @arg dent
arg    : (ARG /SPACE?)* ARG
param  : PARAM? PARAM
dot    : /DOT (op|id)
string : /QUOTE /INDENT (STRING|brace|interp|NEWLINE)* /DEDENT /UNQUOTE
       | /QUOTE         (STRING|brace|interp)*                 /UNQUOTE
interp : INTERP dent
@group : paren | brace | brack
paren  : /LPAREN (expres|@dent /feed) /RPAREN
brace  : /LBRACE (expres|@dent /feed) /RBRACE
brack  : /LBRACK (expres|@dent /feed) /RBRACK
dent   : /INDENT expre /DEDENT
void   : /INDENT void? /DEDENT
feed   : /NEWLINE+
