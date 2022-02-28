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
@exprK : applyk
       | exprJ
@exprJ : applyj
       | applyJ0
       | applyJ1
       | expr1
@expr1 : apply1
       | exprD
@exprD : id
       | atom
       | exprO
@exprO : applyO
       | expr0
@expr0 : apply0
       | e

macro   : OP (/COMMA OP)*
applyM  : m1 | m2 | m3
@m1     : mleft macro
        | mleft? macro (mright | /SPACE (m1d|m2d))
@m1d    : middle macro (mright | /SPACE (m1d|m2d))
@m2d    : middle macro mright? mdent
@m2     : mleft? macro mright? mdent
@m3     : (m1|m2|macro) /feed mbot
@kvnuke : exprkv | (exprkv /SPACE)? nuke
mleft   : (exprK | (exprK /SPACE)? nuke) /SPACE
middle  : kvnuke /SPACE
mright  : /SPACE kvnuke
mdent   : @dent
        | /INDENT (kv2 (/feed /COMMA)? | exprkv) /trail? /DEDENT
mbot    : (expjkv feed)* (expjkv| 
          j0j0_? kvjj0_* expr3)
@expjkv : j0j0_? kvjj0_* (kv0j|kv1|kv2j)
        | j0j0_? kvjj0_* kvjj0
        | j0j0
@exprkv : kvjj0_* (exprK|kv0j|kv1)
@kvjj0  : kv0 j0j0
@kvjj0_ : kv0 j0j0_
@kv0j   : kv0 COMMA?
@kv2j   : kv2 (/feed COMMA)?
@j0j0   : (COMMA (kv0|op|exprD)?)+
@j0j0_  : j0j0 /SPACE

apply3  : expr2 /feed expr3
applyZ  : expr2 /feed kv1
        | expr2 /feed /COMMA
apply2  : exprZ dent
applyj  : exprJ /COMMA
applyJ  : @applyj | applyZ
applyJC : @applyJ | applyJ0
applyJ0 : @applyJ (exprD|op|kv0|kv2)
applyJ1 : expr2 /feed (kv0 (/SPACE kv0)* (/SPACE expr1|kv2)?|kv2)
        | @applyJC         (/SPACE kv0)* /SPACE (expr1|kv0|kv2)
apply1  :           exprD  (/SPACE kv0)* /SPACE (expr1|kv0|kv2)
applyk  : (@applyJC|exprD) (/SPACE kv0)* /SPACE (applyk|kv1)
applyO  : expr0 op
apply0  : exprO ion
        | exprD (group|string)
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
num    : INTEGER | DECIMAL
feed   : /NEWLINE+
string : /QUOTE /INDENT (STRING|group|NEWLINE)* /DEDENT /UNQUOTE
       | /QUOTE /LBRACE (STRING|group|LBRACE|RBRACE)* /RBRACE
       | /QUOTE         (STRING|group)* /UNQUOTE
group  : BQUOTE dent
       | /LPAREN expre /RPAREN
       | /LBRACE expre /RBRACE
       | /LBRACK expre /RBRACK
dent   : /INDENT expre /DEDENT
trail  : /SPACE | /feed | /INDENT /DEDENT
