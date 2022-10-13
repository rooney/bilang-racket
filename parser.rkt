#lang brag

expres : /feeds expr4
@expr4 : /SPACE? expr3 /(SPACE|feeds|pseudent)*
@expr3 : apply3|comma3|speck3|newlk3
       | applyE|macro3|macro2|macro1
       | exprZ
@exprZ : applyZ
       | newlk2|newlkD|newlkI|newlk1|newlkQ|newlkO|newlk0
       | expr2
@expr2 : apply2|applyD
       | comma2|commaD
       | speck2|speckD|speckI
       | speck2|speckD|speckI|speck1|speckQ|speckO|speck0
       | exprI
@exprI : applyI
       | commaI|comma1|commaQ|commaO|comma0
       | expr1
@expr1 : apply1
       | exprQ
@exprQ : applyQ
       | exprC
@exprC : applyC
       | param
       | exprO
@exprO : applyO
       | expr0
@expr0 : apply0
       | applyG
       | grouping
       | dot
       | solo
       | e

applyE : exprZ /feeds expr3
       | exprZ /LINEFEED+ kv3
applyZ : exprZ /LINEFEED+ kvZ+

macro  : @opp (/SPACE kv0)*
@mR    : (exprI|macro1|kv1)
@mL    : (exprI|comma |
          speck|speck1|speckQ|speckO|speck0|
          newlk|newlk1|newlkQ|newlkO|newlk0) /SPACE

macro1 : mL  macro
       | mL? macro /SPACE mR
macro2 : mL? macro (/SPACE (mR COMMA? denty|applyD|commaD|kvD) | denty)
macro3 : mL? macro (/SPACE (mR COMMA? denty|applyD|commaD|kvD) | denty)? /LINEFEED+ expr3
       | mL? macro /SPACE                  (apply3|comma3|kv3)

@comma : (string|commaQ|comma1|applyQ|applyC|apply1) /UNQUOTE-COMMA
       | (comma0|commaQ|comma1|applyQ|applyC|apply1) /COMMA 
@speck : (speck0|speckO|speckQ|speck1|
          newlk0|newlkO|newlkQ|newlk1|
          newlkI|speckI|commaI|applyI|macro1)        /SPACE /COMMA
@block : (newlk2|speck2|comma2|apply2|macro2|
          newlkD|speckD|commaD|applyD|applyZ)        /LINEFEED+ /COMMA
@newlk :               (macro1|macro2|exprZ)         /LINEFEED+ /COMMA

newlk3 : (newlk|newlk0|newlkO|newlkQ)  /SPACE        (kv3|apply3|comma3) | newlk kv3
newlk2 : (block                     )                                      denty
       | (      newlk0|newlkO|newlkQ) (/SPACE        (kv2|apply2|comma2) | denty)
       | (newlk                     )  /SPACE        (kv2|apply2|comma2) | newlk kv2
newlkD : (newlk|newlk0|newlkO|newlkQ)  /SPACE        (kvD|applyD|commaD) | newlk kvD
newlkI : (newlk|newlk0|newlkO|newlkQ)  /SPACE        (kv1|applyI|commaI) | newlk kv1
newlk1 : (newlk|newlk0|newlkO|newlkQ)  /SPACE         exprI
newlkQ : (newlk|newlk0|newlkO|newlkQ)  /SPACE         kv0                | newlk kv0
newlkO : (newlk|newlk0              )                 opp
       | (             newlkO       )  /SPACE /COMMA  opp
newlk0 : (newlk|newlk0|newlkO       )                (dot|grouping|BIND)
speck3 : (speck|speck0|speckO|speckQ)  /SPACE        (kv3|apply3|comma3) | speck kv3
speck2 : (speck|speck0|speckO|speckQ) (/SPACE        (kv2|apply2)|denty) | speck kv2
speckD : (speck|speck0|speckO|speckQ)  /SPACE        (kvD|applyD|commaD) | speck kvD
speckI : (speck|speck0|speckO|speckQ)  /SPACE        (kv1|applyI|commaI) | speck kv1
speck1 : (speck|speck0|speckO|speckQ)  /SPACE         exprI
speckQ : (speck|speck0|speckO|speckQ)  /SPACE         kv0                | speck kv0
speckO : (speck|speck0              )                 opp
       | (             speckO       )  /SPACE /COMMA  opp
speck0 : (speck|speck0|speckO       )                (dot|grouping|BIND)
comma3 : (comma|comma0|commaO|commaQ)  /SPACE        (kv3|apply3)        | comma kv3
comma2 : (comma|comma0|commaO|commaQ) (/SPACE        (kv2|apply2)|denty) | comma kv2
commaD : (comma|comma0|commaO|commaQ)  /SPACE        (kvD|applyD)        | comma kvD
commaI : (comma|comma0|commaO|commaQ)  /SPACE        (kv1|applyI)        | comma kv1
comma1 : (comma|comma0|commaO|commaQ)  /SPACE         expr1
commaQ : (comma|comma0|commaO|commaQ)  /SPACE         kv0                | comma kv0
commaO : (comma|comma0              )                 opp
       | (             commaO|applyO)         /COMMA  opp
comma0 : (comma|comma0|commaO       )                (dot|grouping|BIND)

apply3 : exprQ  /SPACE (apply3|kv3)        | (applyG|grouping) kv3
apply2 : exprQ (/SPACE (apply2|kv2)|denty) | (applyG|grouping) kv2
applyD : exprQ  /SPACE (applyD|kvD)        | (applyG|grouping) kvD
applyI : exprQ  /SPACE (applyI|kv1)        | (applyG|grouping) kv1
apply1 : exprQ  /SPACE expr1
applyQ : exprC (/SPACE kv0)+
applyC :                        grouping e | (applyG|grouping) kv0
applyO : expr0 opp
applyG : (exprO|opp|param) grouping
apply0 : exprO dot
       | opp (dot|e)
       | id (id|nid)

@e : string
   | id
   | nid
   | num

num    : INTEGER | DECIMAL
int    : INTEGER
nid    : INTEGER ID
@id    : ID
@kv0   : key (exprO|opp)
@kv1   : key /SPACE (macro1|exprI)
@kv2   : key /SPACE (macro2|comma2|apply2)
@kvZ   : key /SPACE (macro1|macro2|expr2)|kvD|kv0
@kv3   : key /feeds expr3
@kvD   : key dent
key    : public /COLON (/SPACE? public /COLON)*
param  : (COLON public?)? COLON private?
       | COLON COLON 
op     : OP
opp    : OP | OPP
feeds  : /LINEFEED+ | /BLANKLINE
solo   : /SOLO
dot    : /DOT (opp|op id|id|nid) BIND?
tup    : /DOT (opp|id)?
string : /QUOTE /INDENT (STRING|interp|FEED)* /DEDENT /UNQUOTE
       | /QUOTE         (STRING|interp)*              /UNQUOTE
interp : INTERPOLATE (brace|dent)

@grouping : paren | brace | @bracket
paren     : /LPAREN (expr4|@dent /feeds|opp) /RPAREN
brace     : /LBRACE (expr4|@dent /feeds) /RBRACE
bracket   : /LBRACKET (list|tuple) /RBRACKET

@denty   : dent | blockey
dent     : /INDENT expr4 /DEDENT
blockey  : /INDENT kvZ (/LINEFEED kvZ)* /feeds? /DEDENT
pseudent : /INDENT pseudent? /DEDENT

@clumps : exprC (/SPACE exprC)*
@cs1    : /COMMA /SPACE expr1
@sz     : /SPACE (macro1|macro2|expr2) 
tuple   : /SPACE? clumps (/LINEFEED+ clumps)* /SPACE?
        | /INDENT clumps (/LINEFEED+ clumps)* /DEDENT /LINEFEED
        | /INDENT (tup    (sz|dent) /LINEFEED)* (tup    sz)? /DEDENT /LINEFEED
list    : /INDENT (/COMMA (sz|dent) /LINEFEED)* (/COMMA sz)? /DEDENT /LINEFEED
        | cs1+ (/LINEFEED cs1+)* /SPACE
        | /feeds?

@public  : op? (id|nid|int) opp?
         | opp
@private : op? (id|nid) opp?
         | opp
         | dot
@priv    : private dot
         | priv dot
         | priv opp

