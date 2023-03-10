#lang brag

expres : /feeds? expr4
@expr4 : expr3 /(SPACE|feeds)?
@expr3 : apply3|macro3|macro2|macro1
       | exprZ
@exprZ : comboZ|comboK|combo1|comboQ|comboO|combo
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
@m1    : (expr1|comma1|commaQ|commaO|comma|combo1|comboQ|comboO|combo|close|break) /SPACE
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

@break : (exprK|comboO|comboQ|combo1|macro1)  /NEWLINE /COMMA
@close : (applyZ|commaZ|comboZ|apply2|macro2) /NEWLINE /COMMA
@combo : (comboO|comboQ|combo1)               /SPACE?  /COMMA
@comma : (expr1|commaO|commaQ|comma1)         /SPACE?  /COMMA

comboZ : (break|close|combo|comboO|comboQ) /SPACE (applyZ|kZ)
comboK : (break|close|combo|comboO|comboQ) /SPACE (applyL|applyR|kL|kR)
combo1 : (break|close|combo|comboO|comboQ) /SPACE expr1
comboQ : (break|close|combo|comboO)      (/SPACE k0)+
comboO : (break|close|combo) ops

commaZ : (comma|commaO|commaQ) /SPACE (applyZ|kZ)
commaK : (comma|commaO|commaQ) /SPACE (applyL|applyR|kL|kR)
comma1 : (comma|commaO|commaQ) /SPACE expr1
commaQ : (comma|commaO)       (/SPACE k0)+
commaO :  comma ops

apply3 : expr2 (/NEWLINE /feeds break block)? /feeds expr3
apply2 : expr2 nkey+
       | (expr1|applyR|commaQ|commaO|comboQ|comboO|comma|combo|close) block
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
@kL    : key  /SPACE (applyL|macroL|expr1)
@kR    : key  /SPACE (applyR|macroR)
@kZ    : key (/SPACE (applyZ|macroZ)|dent)
@kB    : kZ|kL|kR block?
xkey   : k0|kB
nkey   : /feeds kB
key    :         (DOT|OP|ID|@int|@dec)+   /COLON
atom   : (/COLON (DOT|OP|ID|@int|@dec)*)? /COLON OP? ID (dot|OP)*
dot    : /DOT (OP|OP? ID|paren)
bind   : @dot @self
@ops   : self|self? (@paren|prop)+
@prop  : bind|dot|op
self   : /LPAREN /RPAREN
this   : /THIS
feeds  : /NEWLINE+

@grouping : @paren | brace | bracket
paren     : /LPAREN (/SPACE? (macro1|macro2|expr2|xkey) /SPACE?|@dent /feeds|OP) /RPAREN
brace     : /LBRACE (/SPACE? (macro1|macro2|expr2|xkey) /SPACE?|@dent /feeds|OP) /RBRACE
bracket   : /LBRACKET /RBRACKET
string    : /QUOTE /INDENT (STRING|interp|NEWLINE)* /DEDENT /UNQUOTE
          | /QUOTE         (STRING|interp)*                 /UNQUOTE
interp    : INTERPOLATE (brace|dent)

@block   : keyblock | dent
keyblock : /INDENT kB nkey* /feeds? /DEDENT
dent     : /INDENT expr4 /DEDENT
