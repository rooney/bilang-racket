#lang brag
expres : /feed? expre
       | /INDENT expre /DEDENT /feed?

@expre : exprK /(SPACE|feed|INDENT DEDENT)?
@exprK : exprJ
@exprJ : applyJ
       | expr1
@expr1 : apply1
       | expr0
@expr0 : e0
       | dot
       | atom
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
atom   : ATOM ATOM?
dot    : /DOT (op | (op? int?)* id)
feed   : /NEWLINE+
string : /QUOTE /INDENT (STRING|braces|interp|NEWLINE)* /DEDENT /UNQUOTE
       | /QUOTE /LBRACE (STRING|braces|interp|LBRACE|RBRACE)* /RBRACE
       | /QUOTE         (STRING|braces|interp)* /UNQUOTE
@group : parens | bracks | braces | interp | string
parens : /LPAREN (expre|@dent /feed) /RPAREN
bracks : /LBRACK (expre|@dent /feed) /RBRACK
braces : /LBRACE (expre|@dent /feed) /RBRACE
dent   : /INDENT expre /DEDENT
interp : BQUOTE dent
