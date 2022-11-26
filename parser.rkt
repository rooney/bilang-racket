#lang brag

expres : /feeds expr4
@expr4 : /SPACE? expr3 /(SPACE|feeds|pseudent)*
@expr3 : apply3|comma3|speck3|newlk3
       | applyE|macro3|macro2|macro1
       | exprZ
@exprZ : applyZ
       | newlk2|newlkD|newlkK|newlk1|newlkQ|newlkO|newlk0
       | expr2
@expr2 : apply2|applyD
       | comma2|commaD
       | speck2|speckD|speckK
       | speck2|speckD|speckK|speck1|speckQ|speckO|speck0
       | exprK
@exprK : applyK
       | commaK|comma1|commaQ|commaO|comma0
       | expr1
@expr1 : apply1
       | exprQ
@exprQ : applyQ
       | expGQ
@expGQ : groupQ
       | param
       | exprG
@exprG : applyG
       | group
       | groupO
       | group0
       | g-dote
       | dot
       | exprO
@exprO : applyO
       | expr0
@expr0 : apply0
       | solo
       | e

applyE : exprZ /feeds expr3
       | exprZ /LINEFEED+ kv3
applyZ : exprZ /LINEFEED+ kvZ+

macro  : @op (/SPACE kv0)*
@mR    : (exprK|macro1|kv1)
@mL    : (exprK|comma |
          speck|speck1|speckQ|speckO|speck0|
          newlk|newlk1|newlkQ|newlkO|newlk0) /SPACE

macro1 : mL  macro
       | mL? macro /SPACE mR
macro2 : mL? macro (/SPACE (mR COMMA? denty|applyD|commaD|kvD) | denty)
macro3 : mL? macro (/SPACE (mR COMMA? denty|applyD|commaD|kvD) | denty)? /LINEFEED+ expr3
       | mL? macro /SPACE                  (apply3|comma3|kv3)

@comma : (string|commaQ|comma1|applyQ|groupQ|apply1) /UNQUOTE-COMMA
       | (comma0|commaQ|comma1|applyQ|groupQ|apply1) /COMMA 
@speck : (speck0|speckO|speckQ|speck1|
          newlk0|newlkO|newlkQ|newlk1|
          newlkK|speckK|commaK|applyK|macro1)        /SPACE /COMMA
@block : (newlk2|speck2|comma2|apply2|macro2|
          newlkD|speckD|commaD|applyD|applyZ)        /LINEFEED+ /COMMA
@newlk :               (macro1|macro2|exprZ)         /LINEFEED+ /COMMA

newlk3 : (newlk|newlk0|newlkO|newlkQ)  /SPACE        (kv3|apply3|comma3) | newlk kv3
newlk2 : (block                     )                                      denty
       | (      newlk0|newlkO|newlkQ) (/SPACE        (kv2|apply2|comma2) | denty)
       | (newlk                     )  /SPACE        (kv2|apply2|comma2) | newlk kv2
newlkD : (newlk|newlk0|newlkO|newlkQ)  /SPACE        (kvD|applyD|commaD) | newlk kvD
newlkK : (newlk|newlk0|newlkO|newlkQ)  /SPACE        (kv1|applyK|commaK) | newlk kv1
newlk1 : (newlk|newlk0|newlkO|newlkQ)  /SPACE         exprK
newlkQ : (newlk|newlk0|newlkO|newlkQ)  /SPACE         kv0                | newlk kv0
newlkO : (newlk|newlk0              )                 op
       | (             newlkO       )  /SPACE /COMMA  op
newlk0 : (newlk|newlk0|newlkO       )                (dot|group|BIND)
speck3 : (speck|speck0|speckO|speckQ)  /SPACE        (kv3|apply3|comma3) | speck kv3
speck2 : (speck|speck0|speckO|speckQ) (/SPACE        (kv2|apply2)|denty) | speck kv2
speckD : (speck|speck0|speckO|speckQ)  /SPACE        (kvD|applyD|commaD) | speck kvD
speckK : (speck|speck0|speckO|speckQ)  /SPACE        (kv1|applyK|commaK) | speck kv1
speck1 : (speck|speck0|speckO|speckQ)  /SPACE         exprK
speckQ : (speck|speck0|speckO|speckQ)  /SPACE         kv0                | speck kv0
speckO : (speck|speck0              )                 op
       | (             speckO       )  /SPACE /COMMA  op
speck0 : (speck|speck0|speckO       )                (dot|group|BIND)
comma3 : (comma|comma0|commaO|commaQ)  /SPACE        (kv3|apply3)        | comma kv3
comma2 : (comma|comma0|commaO|commaQ) (/SPACE        (kv2|apply2)|denty) | comma kv2
commaD : (comma|comma0|commaO|commaQ)  /SPACE        (kvD|applyD)        | comma kvD
commaK : (comma|comma0|commaO|commaQ)  /SPACE        (kv1|applyK)        | comma kv1
comma1 : (comma|comma0|commaO|commaQ)  /SPACE         expr1
commaQ : (comma|comma0|commaO|commaQ)  /SPACE         kv0                | comma kv0
commaO : (comma|comma0              )                 op
       | (             commaO|applyO)         /COMMA  op
comma0 : (comma|comma0|commaO       )                (dot|group|BIND)

apply3 : exprQ  /SPACE (apply3|kv3)        | (applyG|group) kv3
apply2 : exprQ (/SPACE (apply2|kv2)|denty) | (applyG|group) kv2
applyD : exprQ  /SPACE (applyD|kvD)        | (applyG|group) kvD
applyK : exprQ  /SPACE (applyK|kv1)        | (applyG|group) kv1
apply1 : exprQ  /SPACE expr1
applyQ : expGQ (/SPACE kv0)+
applyG : (exprG|op) group
groupQ :                                     (applyG|group) kv0
group0 :                                     (applyG|group) exprO
groupO :                                     (applyG|group|groupO|g-dote|dot) op
g-dote : (g-dote|dot) e                    | (applyG|group|groupO|g-dote|dot|op) dot
applyO : exprO op
apply0 : exprO dot
       | expr0 e
       | op e

nx     : (int|dec) id
@e     : nx
       | int
       | dec
       | id
       | string

int    : INTEGER
dec    : DECIMAL
id     : ID PRIME?
op     : OP PRIME?
@kv0   : key (exprG|op)
@kv1   : key /SPACE (macro1|exprK)
@kv2   : key /SPACE (macro2|comma2|apply2)
@kvZ   : key /SPACE (macro1|macro2|expr2)|kvD|kv0
@kv3   : key /feeds expr3
@kvD   : key dent
key    : (int|dec|id|op|dot)+ /COLON
param  : (/COLON (int|dec|id|op|dot)*)? /COLON id? dot*
dot    : /DOT (OP? ID op|OP? id|op) BIND?
feeds  : /LINEFEED+ | /BLANKLINE
solo   : /SOLO
string : /QUOTE /INDENT (STRING|interp|LINEFEED)* /DEDENT /UNQUOTE
       | /QUOTE         (STRING|interp)*                  /UNQUOTE
interp : INTERPOLATE (brace|dent)

@group  : paren | brace | bracket
paren   : /LPAREN (expr4|@dent /feeds|op) /RPAREN
brace   : /LBRACE (expr4|@dent /feeds) /RBRACE
bracket : /LBRACKET (list|tuple) /RBRACKET

@denty   : dent | blockey
dent     : /INDENT expr4 /DEDENT
blockey  : /INDENT kvZ (/feeds kvZ)* /feeds? /DEDENT
pseudent : /INDENT pseudent? /DEDENT

@ssc  : expGQ (/SPACE expGQ)*
@cs1  : /COMMA /SPACE expr1
@sz   : /SPACE (macro1|macro2|expr2) 
tuple : /SPACE? ssc (/feeds ssc)* /SPACE?
      | /INDENT ssc (/feeds ssc)* /DEDENT /LINEFEED
list  : /INDENT (/COMMA (sz|dent) /LINEFEED)* (/COMMA sz)? /DEDENT /LINEFEED
      | /feeds?
      | cs1+ (/LINEFEED cs1+)* /SPACE?
