#lang brag
return : /NEWLINE? expr4

@expr4 : exprE (/SPACE|/NEWLINE|/INDENT/DEDENT)?
@exprE : applyE3 | applyEZ | applyE1 | applyEO
       | enco
       | expr3
@expr3 : apply3
       | expQ2
@expQ2 : _Q_2
       | apply2
       | expQ1
@expQ1 : _Q_
       | applyCQ
       | expC1
@expC1 : applyC1
       | eco
       | expr1
@expr1 : apply1
       | expCO
@expCO : applyCO
       | exprO
@exprO : applyO
       | expr0
@expr0 : apply0
       | label
       | alias
       | e

applyE3 : enco /NEWLINE expr3
applyEZ : enco /SPACE (_Q_|_Q_2)
        | (enco|applyE1) dent
applyE1 : (enco|applyEO) /SPACE expr1
applyEO : enco (exprO|qwop)
@enco   : exprE /NEWLINE co
        | (applyEO|applyE1) co
apply3  : (applyEO|applyE1|applyEZ|expQ2) /NEWLINE expr3
_Q_2    : qwop dent
        | qwop /SPACE apply2
        | (qwop|expCO) /SPACE _Q_2
apply2  : eco /SPACE _Q_2
        | (eco|expC1) dent
@_Q_    : _Qx | _Q
_Qx     : qwop /SPACE expQ1
        | expCO /SPACE _Qx
@_Q     : xQ | qwop
xQ      : expCO /SPACE _Q
@qwop   : keyword | OP
applyCQ : eco /SPACE _Q_
applyC1 : (eco|applyCO) /SPACE expr1
@eco    : (_Q|applyC1|expr1) co
@co     : /COMMA | COMMA-COLON
apply1  : exprO /SPACE expr1
applyCO : eco (exprO|qwop)
applyO  : begin (exprO|qwop)?
        | keyword (exprO|qwop)
        | expr0 OP
apply0  : expr0 e
        | OP e

@e : INTEGER | DECIMAL
   | string
   | ID
   | prop
   | group
   | undent

@begin : BPAREN | BBRACE | BBRACKET
group : /LPAREN expr4 /RPAREN
      | /LBRACE expr4 /RBRACE
      | /LBRACKET expr4 /RBRACKET
@dent : /INDENT expr4 /DEDENT
@undent : /BACKSLASH dent

alias : label label+
@name : OP? ID OP?
label : COLON (OP|name)?
keyword : (OP|name) COLON
prop : /DOT name

string : /QUOTE (STRING|group|NEWLINE)* /UNQUOTE
       | /QUOTE /INDENT (STRING|group|NEWLINE)* /DEDENT /UNQUOTE
