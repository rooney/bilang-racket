#lang brag

expres : /feeds? expr4
@expr4 : expr3 /(SPACE|feeds)?
@expr3 : apply3|macro3|macro2|macro1
       | contiZ|contiK|conti1|contiQ|contiO|conti
       | expr2
@expr2 : apply2|applyZ|commaZ|comma
       | exprK
@exprK : commaK|comma1|commaQ|commaO
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

@macro : OP (/SPACE k0 (/SPACE? /COMMA)?)*
@m1    : (expr1|comma1|commaQ|commaO|comma|conti1|contiQ|contiO|conti|close|combi) /SPACE
@mL    : expr1 /SPACE
@mR    : /SPACE (exprK|macroL|kL)
@mZ    : /SPACE (applyZ|commaZ|kZ)
@m3    : (mR|mZ)? (nkey+|nkey* /feeds expr3)
macro1 : m1? @macroR | m1 macro
macroL : mL? @macroR | mL macro
macroR :     macro mR
macroZ : mL? macro mZ
macro2 : m1? macro mZ
macro3 : m1? macro m3

@comma : (expr1|commaO|commaQ|comma1)         /SPACE?  /COMMA
@conti : (contiO|contiQ|conti1)               /SPACE?  /COMMA
@combi : (exprK|contiO|contiQ|conti1|macro1)  /NEWLINE /COMMA
@close : (applyZ|commaZ|contiZ|apply2|macro2) /NEWLINE /COMMA

contiZ : (combi|close|conti|contiO|contiQ) /SPACE (applyZ|kZ)
contiK : (combi|close|conti|contiO|contiQ) /SPACE (applyL|applyR|kL|kR)
conti1 : (combi|close|conti|contiO|contiQ) /SPACE expr1
contiQ : (combi|close|conti|contiO)      (/SPACE k0)+
contiO : (combi|close|conti) ops

commaZ : (comma|commaO|commaQ) /SPACE (applyZ|kZ)
commaK : (comma|commaO|commaQ) /SPACE (applyL|applyR|kL|kR)
comma1 : (comma|commaO|commaQ) /SPACE expr1
commaQ : (comma|commaO)       (/SPACE k0)+
commaO :  comma ops

apply3 : expr2 (/NEWLINE /feeds combi block)? /feeds expr3
apply2 : expr2 nkey+
       | (expr1|applyR|commaQ|commaO|contiQ|contiO|comma|conti|close) block
applyZ : exprQ /SPACE (applyZ|kZ)
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
@kL    : key /SPACE (applyL|macroL|expr1)
@kR    : key /SPACE (applyR|macroR)
@kZ    : key /SPACE (applyZ|macroZ)
       | key dentz
@kx    : kZ | k0 (/SPACE k0)*
       | kL | kR block? 
nkey   : /feeds kx
key    :         (DOT|OP|ID|@int|@dec)+   /COLON
atom   : (/COLON (DOT|OP|ID|@int|@dec)*)? /COLON (ID dot*)?
dot    : /DOT (OP|OP? ID|paren)
bind   : @dot @self
@ops   : self | self? (@paren|prop)+
@prop  : bind | dot | op
self   : /LPAREN /RPAREN
this   : /THIS
feeds  : /NEWLINE+

@grouped  : /SPACE? expr4 /SPACE?
          | @dentz /feeds
          | OP
@grouping : @paren | brace | bracket
paren     : /LPAREN grouped /RPAREN
brace     : /LBRACE grouped /RBRACE
bracket   : /LBRACK grouped /RBRACK
string    : /QUOTE /INDENT (STRING|interp|NEWLINE)* /DEDENT /UNQUOTE
          | /QUOTE         (STRING|interp)*                 /UNQUOTE
interp    : INTERPOLATE (brace|dentz)

@block   : keyblock | dentz
keyblock : /INDENT kx nkey* /feeds? /DEDENT
dentz    : /INDENT expr4 /DEDENT
