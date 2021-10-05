#lang brag
return : /NEWLINE? expr4

@expr4 : exprE (/space|/NEWLINE|/INDENT/DEDENT)?
@exprE : applyE3 | applyEZ | applyE1 | applyEO
       | enco
       | expr3
@expr3 : apply3
       | expQz
@expQz : kQz
       | oQz
       | exprK
@exprK : applyK1
       | applyKO
       | eco
       | expr1
@expr1 : apply1
       | exprO
@exprQ : exprO | qwop
@exprO : applyO
       | label
       | alias
       | expr0
@expr0 : apply0
       | e

applyE3 : (enco|applyEO|applyE1|applyEZ) /NEWLINE expr3
applyEZ : (enco|applyEO) /space _Qz
applyE1 : (enco|applyEO) (/space expr1|dent)
applyEO : enco exprQ
@enco   : exprE /NEWLINE co
        | (applyEO|applyE1) co
apply3  : expQz /NEWLINE expr3
kQz     : (eco|applyKO) /space _Qz
oQz     : exprO /space _Qz
@_Qz    : oQz | Qz
Qz      : qwop (/space (expQz|Qz)|dent)
@qwop   : keyword | OP
@eco    : exprK co
@co     : comma | PIPE
applyK1 : (eco|applyKO) (/space expr1|dent)
applyKO : eco exprQ
apply1  : exprO (/space expr1|dent)
applyO  : begin exprQ?
        | keyword exprQ
        | expr0 OP
apply0  : expr0 OP e
        | expr0 e
        | OP e

@e : INTEGER | DECIMAL
   | string
   | ID
   | comment
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
