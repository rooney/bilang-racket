#lang brag

expres : /feeds? expr4
@expr4 : expr3 /(SPACE|feeds|pseudent)*
@expr3 : apply3|comma3|break3
       | applyE|macro3|macro2|macro1|macroB
       | exprZ
@exprZ : applyZ
       | break2|breakD|breakk|break1|breakQ|breakO
       | expr2
@expr2 : apply2|applyD
       |        commaD
       | exprK
@exprK : applyK
       | commak|commaQ|commaO|comma1|comma
       | expr1
@expr1 : apply1
       | exprQ
@exprQ : applyQ
       | expr0
@expr0 : apply0
       | atom
       | exprG
@exprG : applyG|grouping
       | dot|bind|this
       | e

applyE : exprZ (/feeds expr3)+
       | exprZ /LINEFEED kv3
applyZ : exprZ /LINEFEED (kv0|kv1|kvB|kvD)+

@macro : OP (/SPACE kv0 (/SPACE? /COMMA)?)*
@mB    : mR? block | /SPACE (commaD|applyD|kvD)
@mR    :             /SPACE  (exprK|macro1|kv1)
@mL    :                     (expr1|comma1|commaQ|commaO|comma) /SPACE
@mZ    : mL        |               (break1|breakQ|breakO|break) /SPACE

macro1 : mL  macro
       | mL? macroR
macroR :     macro mR
macroB :     macro mB
macro2 : mZ? macro mB
macro3 : mZ? macro (mB|mR)? (lKeys|lKeys? /LINEFEED expr3)
       | mZ? macro /SPACE (comma3|apply3|kv3)

@comma : (expr1|commaQ|commaO|comma1)               /SPACE? /COMMA
@dents : (break2|              apply2|macro2|
          breakD|commaD|applyD|applyZ)              /LINEFEED /COMMA
@break : (exprZ|macro1|macro2|breakO|breakQ|break1) /LINEFEED /COMMA
       | (breakO|breakQ|break1)                     /SPACE? /COMMA

break3 : (break|breakO|breakQ)  /SPACE (kv3|apply3|comma3)
break2 : (dents              )                        block
       | (      breakO|breakQ) (/SPACE (kvB|apply2) | block)
       | (break              )  /SPACE (kvB|apply2)
breakD : (break|breakO|breakQ)  /SPACE (kvD|applyD)
breakk : (break|breakO|breakQ)  /SPACE (kv1|applyK)
break1 : (break|breakO|breakQ)  /SPACE expr1
breakQ : (break|breakO)        (/SPACE kv0)+
breakO :  break ops

comma3 : (comma|commaO|commaQ)  /SPACE (kv3|apply3)
commaD : (comma|commaO|commaQ)  /SPACE (kvD|applyD)
commak : (comma|commaO|commaQ)  /SPACE (kv1|applyK)
comma1 : (comma|commaO|commaQ)  /SPACE expr1
commaQ : (comma|commaO)        (/SPACE kv0)+
commaO :  comma ops

apply3 : exprQ  /SPACE (apply3|kv3)
apply2 : (expr1|comma|comma1|commaO|commaQ) block
applyD : exprQ  /SPACE (applyD|kvD|kvB)
applyK : exprQ  /SPACE (applyK|kv1)
apply1 : exprQ (/SPACE expr1)+
applyQ : expr0 (/SPACE kv0)+
apply0 : (applyG|grouping) e
       | (exprG|op) prop+
applyG : (expr0|op) grouping+
apply  : (times|ID|op) (int|dec|ID|times+)
parse  : (times|ID|op|apply) string
times  : (int|dec) ID
@e     : times|parse|apply|string|int|dec|ID

op     : OP
@int   : INTEGER
@dec   : DECIMAL
@kv0   : key         expr0
@kv1   : key /SPACE (expr1|applyK|macroR)
@kvB   : key /SPACE  macroB
@kv3   : key /feeds  expr3
@kvD   : key         dent
key    :         (DOT|OP|ID|@int|@dec)+   /COLON
atom   : (/COLON (DOT|OP|ID|@int|@dec)*)? /COLON OP? ID (dot|OP)*
dot    : /DOT (OP|OP? ID|paren)
bind   : @dot @self
@ops   : self|self? (@paren|prop)+
@prop  : bind|dot|op
self   : /LPAREN /RPAREN
this   : /THIS

@grouping : @paren | brace | bracket
paren     : /LPAREN (/SPACE? expr4|@dent /feeds|OP) /RPAREN
brace     : /LBRACE (/SPACE? expr4|@dent /feeds) /RBRACE
bracket   : /LBRACKET (list|tuple) /RBRACKET
string    : /QUOTE /INDENT (STRING|interp|LINEFEED)* /DEDENT /UNQUOTE
          | /QUOTE         (STRING|interp)*                  /UNQUOTE
interp    : INTERPOLATE (brace|dent)

@block   : dent | dKeys
dent     : /INDENT expr4 /DEDENT
pseudent : /INDENT pseudent? /DEDENT
dKeys    : /INDENT keys /DEDENT
lKeys    : /LINEFEED keys
@keys    : (kv0|kv1|kvB|kvD) (/feeds (kv0|kv1|kvB|kvD))* /feeds?

@ssc  : expr0 (/SPACE expr0)*
@cs1  : /COMMA /SPACE expr1
@sz   : /SPACE (macroR|macro2|expr2) 
tuple : /SPACE? ssc (/feeds ssc)* /SPACE?
      | /INDENT ssc (/feeds ssc)* /DEDENT /LINEFEED
list  : /INDENT (/COMMA (sz|dent) /LINEFEED)* (/COMMA sz)? /DEDENT /LINEFEED
      | /feeds?
      | cs1+ (/LINEFEED cs1+)* /SPACE?

feeds : /LINEFEED+ | /BLANKLINE
