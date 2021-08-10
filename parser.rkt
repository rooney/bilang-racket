#lang brag

return : /NEWLINE? expr4
@exl23 : apply3 | applyQ2 | apply2 | applyP2 | applyQ1 | apptoQ | qwop

@expr4 : exprK /(SPACE | NEWLINE | INDENT DEDENT)?
@exprK : applyK3
       | applyK2
       | applyK1
       | applyK
       | expr3
@expr3 : apply3
       | expQ2
@expQ2 : applyQ2
       | applyP2
       | apply2
       | expQ1
@expQ1 : applyQ1
       | applyCQ1
       | apptoQ
       | qwop
       | expP1
@expP1 : applyP1
       | expC1
@expC1 : applyC1
       | applyC
       | expr1
@expr1 : apply1
       | expP0
@expP0 : applyP0
       | exprO
@exprO : applyO
       | beginO
       | expr0
@expr0 : apply0
       | label
       | alias
       | e

applyK3 : applyK /NEWLINE expr3
applyK2 : applyK dent
applyK1 : applyK /SPACE expr1
        | applyK1 /SPACE expr1
@applyK : expnCOP | expnC
expnCOP : expnC OP
@expnC : expr4 /NEWLINE /COMMA
apply3 : expQ2 /NEWLINE expr3
applyQ2 : applyC /SPACE applyQ2
        | any0 /SPACE applyQ2
        | qwop /SPACE apply2
        | qwop dent
applyP2 : piped dent
apply2 : expP1 dent
       | begin dent
applyQ1 : qwop /SPACE (expC1|applyCQ1|qwop)
        | any0 /SPACE applyQ1
applyCQ1 : applyC /SPACE (applyQ1|apptoQ|qwop)
apptoQ : expP0 /SPACE (apptoQ|qwop)
@any0 : expP0 | qwop
@expQ0 : exprO | qwop
@qwop : keyword | OP
applyP1 : piped /SPACE (apply1|expP0)
applyC1 : applyP0 /SPACE expr1
        | applyC /SPACE expr1
applyC : expCOP | expC
expCOP : expC OP
@expC : (beginO|apply1|applyC1|applyP1|apptoQ|qwop) /COMMA
apply1 : exprO /SPACE expr1
applyP0 : piped expQ0
@piped : expP1 /NEWLINE? /PIPE
       | exl23 /NEWLINE /PIPE
       | begin /PIPE
beginO : begin expQ0
applyO : keyword expQ0
       | expr0 OP
apply0 : expr0 e
       | OP e

@e : INTEGER | DECIMAL
   | string
   | ID
   | dot
   | group
   | undent

@subexpr : begin
         | expr4
         | dent
@dent : /INDENT /NEWLINE? expr4 /DEDENT
@undent : /BACKSLASH dent

@begin : BPAREN | BBRACE | BBRACKET
group : /LPAREN subexpr /RPAREN
      | /LBRACE subexpr /RBRACE
      | /LBRACKET subexpr /RBRACKET

alias : label label+
@name : OP? ID OP?
label : COLON (OP|name)?
keyword : (OP|name) COLON
dot : /DOT name

string : /QUOTE (STRING|group|NEWLINE)* /UNQUOTE
       | /QUOTE /INDENT (STRING|group|NEWLINE)* /DEDENT /UNQUOTE
