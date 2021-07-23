#lang brag

return : /NEWLINE? expr3

@expr3 : apply3
       | expr2 (/NEWLINE | /INDENT /DEDENT)?

@expr2 : apply2
       | expr1

@expr1 : apply1
       | expr0 

@expr0 : apply0
       | INTEGER
       | DECIMAL
       | STRING
       | keyword | label | ID
       | resolve
       | op
       | prop
       | bracket | group | thunk

apply3 : expr2 /NEWLINE expr3
apply2 : expr1 /INDENT expr3 /DEDENT
apply1 : expr0 /SPACE expr1
apply0 : expr0 expr0

label : COLON ID?
keyword : ID COLON

resolve : DASH ID
op : (OP | DASH)+
prop : expr0? DOT DASH? ID

bracket : /LBRACKET expr3 /RBRACKET
group : /LPAREN expr3 /RPAREN
      | /BACKSLASH /INDENT expr3 /DEDENT
thunk : /LCURLY expr3 /RCURLY 
      | /INDENT expr3 /DEDENT

