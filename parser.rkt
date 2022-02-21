#lang brag
expres : /feed? expre
       | /INDENT expre /DEDENT /feed?

@expre : expr3 /trail?
@expr3 : apply3
       | applyM
       | expr2
@expr2 : apply2
       | exprZ
@exprZ : applyZ
       | exprK
@exprK : applyK
       | exprJ
@exprJ : applyJ
       | applyJ0
       | applyJ1
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

macro   : OP (/COMMA OP)*
@m1     : mleft  macro
        | mleft? macro (mright | /SPACE (m1d|m2d))
@m1d    : middle macro (mright | /SPACE (m1d|m2d))
@m2d    : middle macro mright? mdent
@m2     : mleft? macro mright? mdent
@m3     : (m1|m2|macro) /feed mbot
applyM  : m1 | m2 | m3

exprkv  : kvjj0_* (exprK|kv1|kv0j)
kvnuke  : exprkv | (exprkv /SPACE)? nuke
mleft   : (exprK | (exprK /SPACE)? nuke) /SPACE
middle  : kvnuke /SPACE
mright  : /SPACE kvnuke
mdent   : @dent
        | /INDENT (kv2 (/feed /COMMA)? | exprkv) /trail? /DEDENT
mbot    : (parte feed)* (parte| 
          j0j0_? kvjj0_* expr3)
@parte  : j0j0_? kvjj0_* (kv0j|kv1|kv2j)
        | j0j0_? kvjj0_* kvjj0
        | j0j0

@j0j0   : (COMMA (kv0|op|exprD)?)+
@j0j0_  : j0j0 /SPACE
@kvjj0  : kv0 j0j0
@kvjj0_ : kv0 j0j0_
@kv0j   : kv0 COMMA?
@kv2j   : kv2 (/feed COMMA)?

apply3  : expr2 /feed expr3
applyZ  : expr2 /feed kv1
        | expr2 /feed /COMMA
apply2  : exprZ dent
applyJ  : exprJ /COMMA
applyJZ : applyJ | applyZ
applyK  : (applyJZ|applyJ0|exprB) (/SPACE kv0)* /SPACE (kv1|applyK)
applyJ0 : applyJZ (exprD|op|kv0|kv2)
applyJ1 : expr2 /feed (kv0  (/SPACE kv0)* (/SPACE expr1|kv2)?|kv2)
        | (applyJZ|applyJ0) (/SPACE kv0)* /SPACE (expr1|kv0|kv2)
apply1  : exprB             (/SPACE kv0)* /SPACE (expr1|kv0|kv2)
applyB  : begin (exprD|op)?
applyO  : expr0 op
apply0  : exprO ion
        | exprD group
        | op e

@e : num
   | string
   | group
   | ion

op     : OP
kv0    : @nuke (exprD|op)
kv1    : @nuke /SPACE exprJ
kv2    : @nuke dent
nuke   : (@proton ELECTRON /SPACE?)* @proton ELECTRON
atom   : (ELECTRON @proton?)? ELECTRON (@id|op)?
proton : (OP|ID|num)+
ion    : /DOT (@id|op)
id     : op? num? ID op? ion*
@num   : INTEGER | DECIMAL
string : /QUOTE (QUOTE|STRING|group|UNQUOTE)* /UNQUOTE
       | /QUOTE /INDENT (STRING|group|NEWLINE)* /DEDENT /UNQUOTE

@begin : PAREN | BRACE | BRACKET
group  : BQUOTE dent
       | /LPAREN expre /RPAREN
       | /LBRACE expre /RBRACE
       | /LBRACK expre /RBRACK
dent   : /INDENT expre /DEDENT
feed   : /NEWLINE+
trail  : /SPACE | /feed | /INDENT /DEDENT
