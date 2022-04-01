#lang brag
expres : /feed? expre
       | /INDENT expre /DEDENT /feed?

@expre : exprK /trail?
@exprK : exprJ
@exprJ : applyJ
       | expr1
@expr1 : apply1
       | exprO
@exprO : applyO
       | atom
       | expr0
@expr0 : apply0
       | applyG
       | dot
       | e

applyJ  : expr1 j+
j       : /COMMA (exprO|dot+|dot* op) (/SPACE expr1)?
apply1  : exprO (/SPACE kv0)* /SPACE (kv0|expr1)
applyO  : expr0 op
apply0  : expr0 dot
        | (applyG|group|string|op) e
applyG  : exprO group
        | expr0 string

@e : num
   | string
   | group
   | id

id     : OPID? @num? OPID? ID OPID?
op     : OPID | OP
kv0    : @nuke (exprO|op)
kv1    : @nuke /SPACE expr1
kv2    : @nuke dent
nuke   : (NUKE /SPACE?)* NUKE
atom   : ATOM ATOM?
dot    : /DOT (@op|@id)
num    : INTEGER | DECIMAL
feed   : /NEWLINE+
string : /QUOTE /INDENT (STRING|interp|braces|NEWLINE)* /DEDENT /UNQUOTE
       | /QUOTE /LBRACE (STRING|interp|braces|LBRACE|RBRACE)* /RBRACE
       | /QUOTE         (STRING|interp|braces)* /UNQUOTE
@group : parens | interp | bracks | braces
parens : /LPAREN (expre|@dent /feed) /RPAREN
bracks : /LBRACK (expre|@dent /feed) /RBRACK
braces : /LBRACE (expre|@dent /feed) /RBRACE
dent   : /INDENT expre /DEDENT
trail  : /SPACE | /feed | /INDENT /DEDENT
interp : BQUOTE dent
