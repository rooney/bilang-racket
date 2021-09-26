#lang brag
return : /NEWLINE? expr4

@expr4 : exprE (/space|/NEWLINE|/INDENT/DEDENT)?
@exprE : applyE3 | applyEZ | applyE1 | applyEO
       | enco
       | expr3
@expr3 : apply3
       | exprZ
@exprZ : _Q_
       | eco
       | applyC2
       | expC1
@expC1 : applyC1
       | expr1
@expr1 : apply1
       | expCO
@expCO : applyCO
       | exprO
@exprO : applyO
       | label
       | alias
       | expr0
@expr0 : apply0
       | e

applyE3 : enco /NEWLINE expr3
applyEZ : enco /space _Q_
applyE1 : (enco|applyEO) (/space expr1|dent)
applyEO : enco (exprO|qwop)
@enco   : exprE /NEWLINE co
        | (applyEO|applyE1) co
apply3  : (applyEO|applyE1|applyEZ|exprZ) /NEWLINE expr3
@_Q_    : _Qx | _Q
_Qx     : expCO /space _Qx
        | qwop (/space exprZ|dent)
@_Q     : qwop | xQ
xQ      : expCO /space _Q
@qwop   : keyword | OP
eco    : (applyC1|apply1|applyCO|_Q) co
@co     : comma | PIPE
applyC2 : eco /space _Q_
applyC1 : eco (/space _Q|dent)
        | (eco|applyCO) /space expr1
apply1  : expCO (/space expr1|dent)
applyCO : eco (exprO|qwop)
applyO  : begin (exprO|qwop)?
        | keyword (exprO|qwop)
        | expr0 OP
apply0  : expr0 OP e
        | expr0 e
        | OP e

@e : INTEGER | DECIMAL
   | string
   | comment
   | ID
   | dot
   | grouping
   | undent

@begin : BPAREN | BBRACE | BBRACKET
grouping : /LPAREN expr4 /RPAREN
         | /LBRACE expr4 /RBRACE
         | /LBRACKET expr4 /RBRACKET
         | /BQUOTE dent
dent : /INDENT expr4 /DEDENT
@undent : /BACKSLASH @dent
@space : /SPACE (/BACKSLASH /NEWLINE)?
@comma : /COMMA | /BACKSLASH /NEWLINE

alias : label label+
label : COLON (OP|expr0)?
keyword : (OP|expr0) COLON
dot : /DOT (OP|expr0)

comment : COMMENT
string : /QUOTE (STRING|grouping)* /UNQUOTE
       | /QUOTE /INDENT (STRING|grouping|NEWLINE)* /DEDENT /UNQUOTE
