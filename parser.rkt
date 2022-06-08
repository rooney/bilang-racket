#lang brag
expres : /feed? expre
@expre : exprK /(SPACE|feed|excess)?
@exprK : exprJ
@exprJ : applyJ
       | expr1
@expr1 : apply1
       | expr0
@expr0 : e0
       | dot
       | atom
       | prod
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

@e0 : edot
    | num
    | id

edot   : e0 dot
num    : INTEGER | DECIMAL
int    : INTEGER
id     : ID
op     : OP
kv0    : @nuke (expr0|op)
kv1    : @nuke /SPACE expr1
kv2    : @nuke dent
nuke   : (NUKE /SPACE?)* NUKE
atom   : ATOM? PARAM
dot    : /DOT (op|id)
prod   : /PROTECTED (op|id)
string : /QUOTE /INDENT (STRING|braces|interp|NEWLINE|/LBRACE /NEWLINE /RBRACE)* /DEDENT /UNQUOTE
       | /QUOTE         (STRING|braces|interp)* /UNQUOTE
interp : BQUOTE dent
@group : parens | bracks | braces | interp | string
parens : /LPAREN (expres|@dent /feed) /RPAREN
bracks : /LBRACK (expres|@dent /feed) /RBRACK
braces : /LBRACE (expres|@dent /feed) /RBRACE
dent   : /INDENT expre /DEDENT
excess : /INDENT excess? /DEDENT
feed   : /NEWLINE+
