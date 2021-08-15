#lang brag
return : /NEWLINE? expr4

@expr4 : exprE /(SPACE | NEWLINE | INDENT DEDENT)?
@exprE : applyE3 | applyE2 | applyE1 | applyEQ
       | pipeE2 | pipeE1 | pipeEO | pipeEQ
       | enco
       | expr3
@expr3 : apply3
       | expQ2
@expQ2 : _Q_2
       | apply2
       | expQ1
@expQ1 : _Q_
       | pipeQ
       | applyCQ
       | expC1
@expC1 : applyC1
       | pipe1
       | co
       | expr1
@expr1 : apply1
       | atomO
@atomO : pipeO
       | exprO
@exprO : applyO
       | expr0
@expr0 : apply0
       | label
       | alias
       | e

applyE3 : enco /NEWLINE expr3
applyE2 : enco /SPACE _Q_2
        | enco dent
        | applyE1 dent
pipeE2  : pipen /SPACE _Q_2
        | pipen dent
        | pipeE1 dent
applyE1 : enco /SPACE expr1
pipeE1  : pipen /SPACE expr1
pipeEO  : pipen (exprO|qwop)
pipeEQ  : pipen /SPACE _Q_
applyEQ : enco /SPACE _Q_
enco    : (applyE1|pipeE1) /COMMA OP?
        | expr4 /NEWLINE /COMMA OP?
pipen   : (applyE1|pipeE1) /COMMA-COLON
        | expr4 /NEWLINE /COMMA-COLON
apply3  : expQ2 /NEWLINE expr3
        | (applyE1|applyE2|pipeE1|pipeE2) /NEWLINE expr3
_Q_2    : qwop dent
        | qwop /SPACE apply2
        | (qwop|atomO) /SPACE _Q_2
apply2  : (piped|co) /SPACE _Q_2
        | (piped|expC1) dent
; expQ1 :
@_Q_    : _Qx | _Q
_Qx     : qwop /SPACE expQ1
        | atomO /SPACE _Qx
@_Q     : xQ | qwop
xQ      : atomO /SPACE _Q
@qwop   : keyword | OP
pipeQ   : piped /SPACE _Q_
applyCQ : co /SPACE _Q_
applyC1 : (co|pipeO) /SPACE expr1
pipe1   : piped /SPACE expr1
co      : (_Q|pipe1|applyC1|apply1|begin atomO) /COMMA (prop|OP)?
piped   : (_Q|pipe1|applyC1|expr1) COMMA-COLON
apply1  : exprO /SPACE expr1
; atomO :
pipeO   : piped (exprO|qwop)
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
