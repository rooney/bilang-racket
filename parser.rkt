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
       | exprB
@exprB : applyB
       | exprO
@exprO : applyO
       | obtain
       | expr0
@expr0 : apply0
       | applyG
       | grouping
       | dot
       | solo
       | e

applyE : exprZ /feeds expr3
       | exprZ /FEED+ kv3
applyZ : exprZ /FEED+ kvZ

macro  : @op (/SPACE kv0)*
@mR    : (exprI|macro1|kv1)
@mL    : (exprI|comma |
          speck|speck1|speckQ|speckO|speck0|
          newlk|newlk1|newlkQ|newlkO|newlk0) /SPACE

macro1 : mL  macro
       | mL? macro /SPACE mR
macro2 : mL? macro (/SPACE (mR COMMA? denty|applyD|commaD|kvD) | denty)
macro3 : mL? macro (/SPACE (mR COMMA? denty|applyD|commaD|kvD) | denty)? /FEED+ expr3
       | mL? macro /SPACE                  (apply3|comma3|kv3)

@comma : (string|commaQ|comma1|applyQ|applyB|apply1) /UNQUOTE-COMMA
       | (comma0|commaQ|comma1|applyQ|applyB|apply1) /COMMA 
@speck : (speck0|speckO|speckQ|speck1|
          newlk0|newlkO|newlkQ|newlk1|
          newlkI|speckI|commaI|applyI|macro1)        /SPACE /COMMA
@block : (newlk2|speck2|comma2|apply2|macro2|
          newlkD|speckD|commaD|applyD|applyZ)        /FEED+ /COMMA
@newlk :               (macro1|macro2|exprZ)         /FEED+ /COMMA

newlk3 : (newlk|newlk0|newlkO|newlkQ)  /SPACE        (kv3|apply3|comma3) | newlk kv3
newlk2 : (block                     )                                      denty
       | (      newlk0|newlkO|newlkQ) (/SPACE        (kv2|apply2|comma2) | denty)
       | (newlk                     )  /SPACE        (kv2|apply2|comma2) | newlk kv2
newlkD : (newlk|newlk0|newlkO|newlkQ)  /SPACE        (kvD|applyD|commaD) | newlk kvD
newlkI : (newlk|newlk0|newlkO|newlkQ)  /SPACE        (kv1|applyI|commaI) | newlk kv1
newlk1 : (newlk|newlk0|newlkO|newlkQ)  /SPACE         exprI
newlkQ : (newlk|newlk0|newlkO|newlkQ)  /SPACE         kv0                | newlk kv0
newlkO : (newlk|newlk0              )                 op
       | (             newlkO       )  /SPACE /COMMA  op
newlk0 : (newlk|newlk0|newlkO       )                (dot|grouping|BIND)
speck3 : (speck|speck0|speckO|speckQ)  /SPACE        (kv3|apply3|comma3) | speck kv3
speck2 : (speck|speck0|speckO|speckQ) (/SPACE        (kv2|apply2)|denty) | speck kv2
speckD : (speck|speck0|speckO|speckQ)  /SPACE        (kvD|applyD|commaD) | speck kvD
speckI : (speck|speck0|speckO|speckQ)  /SPACE        (kv1|applyI|commaI) | speck kv1
speck1 : (speck|speck0|speckO|speckQ)  /SPACE         exprI
speckQ : (speck|speck0|speckO|speckQ)  /SPACE         kv0                | speck kv0
speckO : (speck|speck0              )                 op
       | (             speckO       )  /SPACE /COMMA  op
speck0 : (speck|speck0|speckO       )                (dot|grouping|BIND)
comma3 : (comma|comma0|commaO|commaQ)  /SPACE        (kv3|apply3)        | comma kv3
comma2 : (comma|comma0|commaO|commaQ) (/SPACE        (kv2|apply2)|denty) | comma kv2
commaD : (comma|comma0|commaO|commaQ)  /SPACE        (kvD|applyD)        | comma kvD
commaI : (comma|comma0|commaO|commaQ)  /SPACE        (kv1|applyI)        | comma kv1
comma1 : (comma|comma0|commaO|commaQ)  /SPACE         expr1
commaQ : (comma|comma0|commaO|commaQ)  /SPACE         kv0                | comma kv0
commaO : (comma|comma0              )                 op
       | (             commaO|applyO)         /COMMA  op
comma0 : (comma|comma0|commaO       )                (dot|grouping|BIND)

apply3 : exprQ  /SPACE (apply3|kv3)        | (applyG|grouping) kv3
apply2 : exprQ (/SPACE (apply2|kv2)|denty) | (applyG|grouping) kv2
applyD : exprQ  /SPACE (applyD|kvD)        | (applyG|grouping) kvD
applyI : exprQ  /SPACE (applyI|kv1)        | (applyG|grouping) kv1
apply1 : exprQ  /SPACE expr1
applyQ : exprQ  /SPACE kv0
applyB :                        grouping e | (applyG|grouping) kv0
applyO : expr0  op
applyG : (exprO|op) grouping
apply0 : exprO dot
       | op (e|dot)
       | id (id|nid)

@e : string
   | id
   | nid
   | num

num    : INTEGER | DECIMAL
int    : INTEGER
nid    : INTEGER ID
@id    : ID
@kv0   : key (exprO|op)
@kv1   : key /SPACE (macro1|exprI)
@kv2   : key /SPACE (macro2|comma2|apply2)
@kvZ   : key /SPACE (macro1|macro2|expr2)|kvD|kv0
@kv3   : key /feeds expr3
@kvD   : key dent
key    : pub /COLON (/SPACE? pub /COLON)*
obtain : (/COLON pub)? /COLON priv?
op     : OP
feeds  : /FEED+ | /BLANKLINE
solo   : /SOLO
dot    : /DOT (op|id|op id|nid) BIND?
tup    : /DOT (op|id)?
string : /QUOTE /INDENT (STRING|interp|FEED)* /DEDENT /UNQUOTE
       | /QUOTE         (STRING|interp)*              /UNQUOTE
interp : INTERPOLATE (brace|dent)

@grouping : paren | brace | @bracket
paren     : /LPAREN (expr4|@dent /feeds|op) /RPAREN
brace     : /LBRACE (expr4|@dent /feeds) /RBRACE
bracket   : /LBRACKET (list|tuple) /RBRACKET

@denty   : dent | blockey
dent     : /INDENT expr4 /DEDENT
blockey  : /INDENT kvZ (/FEED kvZ)* /DEDENT
pseudent : /INDENT pseudent? /DEDENT

pub      : pubname op?
priv     : privname op?
pubname  : pub (dot|int)
         | op|dot|id|nid|int
         | op (dot|id|nid|int)
privname : priv dot
         | op|dot|id|nid
         | op (dot|id|nid)

@Bs    : exprB (/SPACE exprB)*
@cs1   : /COMMA /SPACE expr1
@sz    : /SPACE (macro1|macro2|expr2) 
tuple  : /SPACE? Bs (/FEED+ Bs)* /SPACE?
       | /INDENT Bs (/FEED+ Bs)* /DEDENT /FEED
       | /INDENT (tup    (sz|dent) /FEED)* (tup    sz)? /DEDENT /FEED
list   : /INDENT (/COMMA (sz|dent) /FEED)* (/COMMA sz)? /DEDENT /FEED
       | cs1+ (/FEED cs1+)* /SPACE
       | /feeds?
