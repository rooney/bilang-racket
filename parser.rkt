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
       |        commaD
       |        speckD
       | exprL
@exprL : speckK|speckQ|speck0|speck1|speck
       | exprK
@exprK : applyK
       | commaK|commaQ|comma0|comma1|comma
       | expr1
@expr1 : apply1
       | exprQ
@exprQ : applyQ
       | exprC
@exprC : groupQ
       | symbol
       | exprG
@exprG : applyG
       | group
       | group0
       | expr0
@expr0 : apply0
       | self
       | prop
       | e

applyE : exprZ /feeds expr3
       | exprZ /LINEFEED+ kv3
applyZ : exprZ /LINEFEED+ kvZ+

macro  : OP (/SPACE kv0)*
@mR    : (exprK|macro1|kv1)
@mL    : (exprK|comma |
          speck|speck1|speckQ|speck0|
          newlk|newlk1|newlkQ|newlkO|newlk0) /SPACE

macro1 : mL  macro
       | mL? macro /SPACE mR
macro2 : mL? macro (/SPACE (mR COMMA? denty|applyD|commaD|kvD) | denty)
macroD :     macro (/SPACE (mR COMMA? denty|applyD|commaD|kvD) | denty)
macro3 : mL? macro (/SPACE (mR COMMA? denty|applyD|commaD|kvD) | denty)? /LINEFEED+ expr3
       | mL? macro /SPACE                  (apply3|comma3|kv3)

@comma : exprK                 /COMMA
@speck : (exprL|macro1) /SPACE /COMMA
@block : (newlk2|              apply2|macro2|
          newlkD|speckD|commaD|applyD|applyZ)        /LINEFEED+ /COMMA
@newlk :               (macro1|macro2|exprZ)         /LINEFEED+ /COMMA

newlk3 : (newlk|newlk0|newlkO|newlkQ)  /SPACE        (kv3|apply3|comma3) | newlk kv3
newlk2 : (block                     )                                      denty
       | (      newlk0|newlkO|newlkQ) (/SPACE        (kv2|apply2)        | denty)
       | (newlk                     )  /SPACE        (kv2|apply2)        | newlk kv2
newlkD : (newlk|newlk0|newlkO|newlkQ)  /SPACE        (kvD|applyD|commaD) | newlk kvD
newlkK : (newlk|newlk0|newlkO|newlkQ)  /SPACE        (kv1|applyK|commaK) | newlk kv1
newlk1 : (newlk|newlk0|newlkO|newlkQ)  /SPACE         exprK
newlkQ : (newlk|newlk0|newlkO|newlkQ)  /SPACE         kv0                | newlk kv0
newlkO : (newlk|newlk0              )                 OP
       | (             newlkO       )  /SPACE /COMMA  OP
newlk0 : (newlk|newlk0|newlkO       )                (dot|group|BIND)

speck3 : (speck|speck0|speckQ)  /SPACE (kv3|apply3|comma3) | speck kv3
speckD : (speck|speck0|speckQ)  /SPACE (kvD|applyD|commaD) | speck kvD
speckK : (speck|speck0|speckQ)  /SPACE (kv1|applyK|commaK) | speck kv1
speck1 : (speck|speck0|speckQ)  /SPACE      exprK
speckQ : (speck|speck0|speckQ) (/SPACE  kv0)+              | speck kv0
speck0 :                                                     speck (exprC|props)

comma3 : (comma|comma0|commaQ)  /SPACE (kv3|apply3)        | comma kv3
commaD : (comma|comma0|commaQ)  /SPACE (kvD|applyD)        | comma kvD
commaK : (comma|comma0|commaQ)  /SPACE (kv1|applyK)        | comma kv1
comma1 : (comma|comma0|commaQ)  /SPACE      expr1
commaQ : (comma|comma0|commaQ) (/SPACE  kv0)+              | comma kv0
comma0 :                                                     comma (exprC|props)

apply3 : exprQ  /SPACE (apply3|kv3) | (applyG|group) kv3
apply2 : exprL denty
applyD : exprQ  /SPACE (applyD|kvD) | (applyG|group) kvD
applyK : exprQ  /SPACE (applyK|kv1) | (applyG|group) kv1
apply1 : exprQ  /SPACE expr1
applyQ : exprC (/SPACE kv0)+
groupQ :                              (applyG|group) kv0
applyG : (exprG|OP) group
group0 :                              (applyG|group) (expr0|props)
apply0 : (expr0|OP) props
       | OP e
@e     : int|dec|ID|timed
timed  : (int|dec) ID

int    : INTEGER
dec    : DECIMAL
@kv0   : key (exprC|OP)
@kv1   : key /SPACE (macro1|exprK)
@kv2   : key /SPACE (macro2|apply2)
@kvZ   : key /SPACE (macro1|macro2|expr2)|kvD|kv0
@kv3   : key /feeds expr3
@kvD   : key (/SPACE macroD|dent)
key    :         (@int|@dec|OP|ID|DOT)+   /COLON
symbol : (/COLON (@int|@dec|OP|ID|DOT)*)? /COLON OP? ID? (dot|op)*
dot    : /DOT (OP|OP? ID)
prop   : @dot bind?
props  : (@prop|OP)+
bind   : /LPAREN /SPACE? kv0? (/SPACE kv0)* kv1? /SPACE? /RPAREN
self   : /SELF

@group  : paren | brace | bracket | string
@paren  : /LPAREN (expr4|@dent /feeds|OP) /RPAREN
brace   : /LBRACE (expr4|@dent /feeds) /RBRACE
bracket : /LBRACKET (list|tuple) /RBRACKET
string  : /QUOTE /INDENT (STRING|interp|LINEFEED)* /DEDENT /UNQUOTE
        | /QUOTE         (STRING|interp)*                  /UNQUOTE
interp  : INTERPOLATE (brace|dent)

@denty   : dent | blockey
dent     : /INDENT expr4 /DEDENT
blockey  : /INDENT kvZ (/feeds kvZ)* /feeds? /DEDENT
pseudent : /INDENT pseudent? /DEDENT

@ssc  : exprC (/SPACE exprC)*
@cs1  : /COMMA /SPACE expr1
@sz   : /SPACE (macro1|macro2|expr2) 
tuple : /SPACE? ssc (/feeds ssc)* /SPACE?
      | /INDENT ssc (/feeds ssc)* /DEDENT /LINEFEED
list  : /INDENT (/COMMA (sz|dent) /LINEFEED)* (/COMMA sz)? /DEDENT /LINEFEED
      | /feeds?
      | cs1+ (/LINEFEED cs1+)* /SPACE?

feeds : /LINEFEED+ | /BLANKLINE
