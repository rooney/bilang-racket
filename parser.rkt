#lang brag

expres : /NEWLINE* expr4
@expr4 : /SPACE? expr3 /(SPACE|NEWLINE|pseudent)*
@expr3 : apply3
       | exprZ
@exprZ : applyZ
       | exprS
@exprS : applyS
       | split|split0|splitO|splitB|splitQ|splitI
       | expr2
@expr2 : apply2
       | exprM
@exprM : applyM
       | exprF
@exprF : func
       | applyF
       | applyP
       | exprC
@exprC : comma|comma0|commaO|commaB|commaQ|comma1
       | coming
       | expr1
@expr1 : apply1
       | exprQ
@exprQ : applyQ
       | param
       | exprO
@exprO : applyO
       | expr0
@expr0 : apply0
       | applyB
       | bracket
       | dot
       | e

apply3 : exprZ /NEWLINE expr3
applyZ : exprZ /NEWLINE (kv0|kv3)

macro  : @op (/SPACE kv0)*

applyM : ((split|split0|splitO|splitB|splitQ|exprC)  /SPACE)? macro  /SPACE kv1     dent?
       | ((split|split0|splitO|splitB|splitQ|exprC)  /SPACE)? macro (/SPACE exprM)? dent
       | ((split|split0|splitO|splitB|splitQ|exprC)  /SPACE)? macro  /SPACE exprM
       |  (split|split0|splitO|splitB|splitQ|exprC)  /SPACE   macro
applyS :  (split|split0|splitO|splitB|splitQ)       (/SPACE (apply2|applyS)|dent)
apply2 :  (comma|comma0|commaO|commaB|commaQ|exprQ) (/SPACE  apply2        |dent)
applyF :  (comma|comma0|commaO|commaB|commaQ|exprQ)  /SPACE (applyF|func)
applyP :  (comma|comma0|commaO|commaB|commaQ|exprQ)  /SPACE (applyP|kv2)
       |  (comma|applyB|bracket) kv2

func   : /FUNCTION expr2
coming : /COMING
@comma : exprC /COMMA
@split : exprS /SPACE /COMMA
       | exprZ /NEWLINE /COMMA

splitI : (split|split0|splitO|splitB|splitQ) /SPACE (exprF|kv2) 
       |  split kv2
splitQ :  split (kv0|e)
       | (split|split0|splitO|splitB|splitQ) /SPACE kv0
split0 : (split|split0|splitO) dot
splitO : (split|split0) op
splitB : (split|split0 op?) bracket

comma1 : (comma|comma0|commaO|commaB|commaQ) /SPACE expr1
commaQ :  comma (kv0|e)
       | (comma|comma0|commaO|commaB|commaQ) /SPACE kv0
comma0 : (comma|comma0|commaO) dot
commaO : (comma|comma0) op
commaB : (comma|comma0 op?) bracket

apply1 : exprQ /SPACE expr1
applyQ : exprQ /SPACE kv0
       | (applyB|bracket) kv0
       | bracket e

applyB : (exprO|op|param) bracket
applyO : expr0 op
apply0 : exprO dot
       | op (e|dot)
       | int id

@e : id
   | string
   | num

num    : INTEGER | DECIMAL
int    : INTEGER
op     : OP
@id    : ID
@kv0   : key (exprO|op|dent)
@kv1   : key /SPACE exprM
@kv2   : key /SPACE expr2
@kv3   : key /SPACE exprS
key    : KEY (/SPACE? KEY)*
param  : PARAM PARAM?
dot    : /DOT (op|id) BIND?
string : /QUOTE /INDENT (STRING|interpol|NEWLINE)* /DEDENT /UNQUOTE
       | /QUOTE         (STRING|interpol)*                 /UNQUOTE
interpol : INTERPOLATE (curly|dent)
@bracket : paren | curly | square
paren    : /LPAREN (expr4|@dent /NEWLINE+|op) /RPAREN
curly    : /LCURLY (expr4|@dent /NEWLINE+) /RCURLY
square   : /LSQUARE (expr4?|@dent /NEWLINE+) /RSQUARE
dent     : /INDENT expr4 /DEDENT
pseudent : /INDENT pseudent? /DEDENT
