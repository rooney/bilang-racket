#lang brag

expres : /feeds? expr4
@expr4 : expr3 /(SPACE|feeds)?
@expr3 : applyE|macroE|macroZ|macroL
       | exprZ
@exprZ : applyB
       | applyN
       | comma|cont
       | cont2|comma2|apply2
       | contL|contR|cont1|contQ|contO
       | exprK
@exprK : commaL|commaR|comma1|commaQ|commaO
       | applyL|applyR
       | expr1
@expr1 : apply1
       | exprQ
@exprQ : applyQ
       | expr0
@expr0 : apply0
       | atom
       | exprG
@exprG : applyG|grouping
       | this|dot|bind
       | e

applyE : exprZ /feeds expr3
applyN : exprZ nkey+
applyB : (expr1|applyR
         |contQ|contO|cont|close
         |commaQ|commaO|comma) block
breakB : break block

@macro : OP (/SPACE k0 (/SPACE? /COMMA)?)*
@m1    : expr1 /SPACE
@mL    : (expr1|comma1|commaQ|commaO|comma|cont1|contQ|contO|cont|close|break) /SPACE
@mR    : /SPACE (macro1|exprK|kL)
@m2    : /SPACE (apply2|comma2|k2)
@m3    : (mR|m2)? (nkey+|nkey* /feeds expr3)
macro1 : m1? @macroR | m1 macro
macroL : mL? @macroR | mL macro
macroR :     macro mR
macro2 : m1? macro m2
macroZ : mL? macro m2
macroE : mL? macro m3

@break : (exprK|contO|contQ|cont1|macroL)           /NEWLINE /COMMA
@close : (apply2|comma2|cont2|applyB|applyN|macroZ) /NEWLINE /COMMA
@cont  : (contO|contQ|cont1)                        /SPACE?  /COMMA
@comma : (expr1|commaO|commaQ|comma1)               /SPACE?  /COMMA

cont2  : (break|close|cont|contO|contQ) /SPACE (apply2|k2)
contL  : (break|close|cont|contO|contQ) /SPACE (applyL|kL)
contR  : (break|close|cont|contO|contQ) /SPACE (applyR|kR)
cont1  : (break|close|cont|contO|contQ) /SPACE  expr1
contQ  : (break|close|cont|contO)      (/SPACE         k0)+
contO  : (break|close|cont) ops

comma2 : (comma|commaO|commaQ) /SPACE (apply2|k2)
commaL : (comma|commaO|commaQ) /SPACE (applyL|kL)
commaR : (comma|commaO|commaQ) /SPACE (applyR|kR)
comma1 : (comma|commaO|commaQ) /SPACE  expr1
commaQ : (comma|commaO)       (/SPACE         k0)+
commaO :  comma ops

apply2 : exprQ /SPACE (apply2|k2)
applyL : exprQ /SPACE (applyL|kL)
applyR : exprQ /SPACE (applyR|kR)
apply1 : exprQ (/SPACE expr1)+
applyQ : expr0 (/SPACE k0)+
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
@k0    : key expr0
@kL    : key  /SPACE (applyL|macro1|expr1)
@kR    : key  /SPACE (applyR|macroR)
@k2    : key (/SPACE (macro2|apply2)|dent)
@kB    : k2|kL|kR block?
nkey   : /feeds kB
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
bracket   : /LBRACKET /RBRACKET
string    : /QUOTE /INDENT (STRING|interp|NEWLINE)* /DEDENT /UNQUOTE
          | /QUOTE         (STRING|interp)*                  /UNQUOTE
interp    : INTERPOLATE (brace|dent)

dent     : /INDENT expr4 /DEDENT
keyblock : /INDENT kB nkey* /feeds? /DEDENT
@block   : dent | keyblock

feeds : /NEWLINE+
