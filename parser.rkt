#lang brag

expres : /feeds expr4
@expr4 : /SPACE? exprE /(SPACE|feeds|pseudent)*
@exprE : applyE|commaE|whiscE|macroE
       | applyF|macro3
       | expr3
@expr3 : apply3
       | exprZ
@exprZ : macro2|macro1
       | expr2
@expr2 : apply2|applyD
       | comma2|commaD
       | whisc2|whiscD|whiscI
       | exprH
@exprH : whisc1|whisc0|whiscO|whiscQ
       | exprI
@exprI : applyI|commaI
       | comma1|comma0|commaO|commaQ
       | expr1
@expr1 : apply1
       | exprQ
@exprQ : applyQ
       | exprG
@exprG : applyG
       | exprO
@exprO : applyO
       | obtain
       | expr0
@expr0 : apply0
       | applyB
       | bracket
       | dot
       | solo
       | e

applyF : (expr2|apply3) /feeds exprE
       | expr3 /FEED+ kvE
apply3 : expr3 /FEED+ (kvZ|kv0|kvD)

macro  : @op (/SPACE kv0)*
@mL    : (exprH|whisc|comma) /SPACE
@mR    : (exprI|kv1|macro1)

macroE : mL? macro /SPACE                 (applyE|commaE|kvE)
macro3 : mL? macro (/SPACE (mR COMMA? dent|applyD|commaD|kvD) | dent)? /FEED+ exprE
macro2 : mL? macro (/SPACE (mR COMMA? dent|applyD|commaD|kvD) | dent)
macro1 : mL? macro /SPACE mR
       | mL  macro

@comma : (comma0|commaQ|comma1|applyQ|applyG|apply1)               /COMMA
       | (string|commaQ|comma1|applyQ|applyG|apply1)               /UNQUOTE-COMMA 
@specc : (whiscI|commaI|applyI|whiscQ|whiscO|whisc0|whisc1|macro1) /SPACE /COMMA
@blocc : (apply3|apply2|applyD|comma2|commaD|whisc2|whiscD|macro2) /FEED+ /COMMA
@whisc :  specc|                                           expr3   /FEED+ /COMMA
whisc2 : (specc|blocc)                                                   dent
       | (      whisc0|whiscO|whiscQ)(/SPACE       (kv2|apply2|comma2) | dent)
       | (whisc                     ) /SPACE       (kv2|apply2|comma2) | whisc kv2
whiscD : (whisc|whisc0|whiscO|whiscQ) /SPACE       (kvD|applyD|commaD) | whisc kvD
whiscE : (whisc|whisc0|whiscO|whiscQ) /SPACE       (kvE|applyE|commaE) | whisc kvE
whiscI : (whisc|whisc0|whiscO|whiscQ) /SPACE       (kv1|applyI|commaI) | whisc kv1
whisc1 : (whisc|whisc0|whiscO|whiscQ) /SPACE        exprI
whiscQ : (whisc|whisc0|whiscO|whiscQ) /SPACE        kv0                | whisc kv0
whiscO : (whisc|whisc0              )               op
       | (             whiscO       ) /SPACE /COMMA op
whisc0 : (whisc|whisc0|whiscO       )              (dot|bracket|BIND)
comma2 : (comma|comma0|commaO|commaQ)(/SPACE       (kv2|apply2)|dent) | comma kv2
commaD : (comma|comma0|commaO|commaQ) /SPACE       (kvD|applyD)       | comma kvD
commaE : (comma|comma0|commaO|commaQ) /SPACE       (kvE|applyE)       | comma kvE
commaI : (comma|comma0|commaO|commaQ) /SPACE       (kv1|applyI)       | comma kv1
comma1 : (comma|comma0|commaO|commaQ) /SPACE        expr1
commaQ : (comma|comma0|commaO|commaQ) /SPACE        kv0               | comma kv0
commaO : (comma|comma0              )               op
       | (             commaO|applyO)        /COMMA op
comma0 : (comma|comma0|commaO       )              (dot|bracket|BIND)

apply2 : exprQ (/SPACE (apply2|kv2)|dent) | (applyB|bracket) kv2
applyD : exprQ  /SPACE (applyD|kvD)       | (applyB|bracket) kvD
applyE : exprQ  /SPACE (applyE|kvE)       | (applyB|bracket) kvE
applyI : exprQ  /SPACE (applyI|kv1)       | (applyB|bracket) kv1
apply1 : exprQ  /SPACE expr1
applyQ : exprQ  /SPACE kv0
applyG :                        bracket e | (applyB|bracket) kv0
applyO : expr0  op
applyB : (exprO|op) bracket
apply0 : exprO dot
       | op (e|dot)
       | (id|nid) (id|nid)

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
@kvZ   : key /SPACE exprZ
@kvE   : key /feeds exprE
@kvD   : key dent
key    : KEYWORD (/SPACE? KEYWORD)*
obtain : PARAM PARAM?
op     : OP
feeds  : /FEED+ | /BLANKLINE
solo   : /SOLO
dot    : /DOT (op|id) BIND?
string : /QUOTE /INDENT (STRING|interpol|FEED)* /DEDENT /UNQUOTE
       | /QUOTE         (STRING|interpol)*              /UNQUOTE
interpol : INTERPOLATE (curly|dent)
@bracket : paren | curly | @square
paren    : /LPAREN (expr4|@dent /feeds|op) /RPAREN
curly    : /LCURLY (expr4|@dent /feeds) /RCURLY
square   : /LSQUARE (list|tuple) /RSQUARE
dent     : /INDENT expr4 /DEDENT
pseudent : /INDENT pseudent? /DEDENT

@gop  : exprG 
      | /LPAREN /SPACE? op /SPACE? /RPAREN
@gops : gop (/SPACE gop)*
tuple : /SPACE? gops /SPACE?
      | /INDENT gops (/FEED gops)* /DEDENT /FEED
list  : /COMMA /SPACE expr1 (/COMMA /SPACE expr1)* /SPACE?
