#lang brag

return : /NEWLINE? expr5

@expr5 : apply5
       | expr4 (/NEWLINE | /INDENT /DEDENT)?

@expr4 : apply4
       | expr3

@expr3 : apply3
       | expr2

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
       | prop
       | bracket | group | thunk

apply5 : expr4 /NEWLINE expr5
apply4 : (expr0|op) /SPACE (apply4|apply3|op)
apply3 : op (/SPACE expr2 | /INDENT expr5 /DEDENT)
apply2 : expr1 /INDENT expr5 /DEDENT
apply1 : expr0 /SPACE expr1
apply0 : expr0 expr0
       | expr0 op
       | op expr0

keyword : ID COLON
label : COLON ID?
resolve : DASH ID
prop : expr0? DOT DASH? ID
op : (OP | DASH)+

bracket : /LBRACKET expr5 /RBRACKET
group : /LPAREN expr5 /RPAREN
      | /BACKSLASH /INDENT expr5 /DEDENT
thunk : /LCURLY expr5 /RCURLY 
      | /INDENT expr5 /DEDENT

