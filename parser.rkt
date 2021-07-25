#lang brag

return : /NEWLINE? expr4

@expr4 : apply4
       | expr3 (/NEWLINE | /INDENT /DEDENT)?

@expr3 : apply3
       | expr2
       | op

@expr2 : apply2
       | expr1

@expr1 : apply1
       | expr0 

@expr0 : apply0
       | num
       | STRING
       | keyword | label | ID | -id
       | prop
       | grouping

apply4 : expr3 /NEWLINE expr4
apply3 : expr0 /SPACE (apply3|op)
       | op /SPACE expr3
       | op dent
apply2 : expr1 dent
apply1 : expr0 /SPACE expr1
apply0 : expr0 expr0
       | expr0 op
       | op expr0

@num : INTEGER | DECIMAL
@grouping : paren | brace | brakt | undent

dent : INDENT expr4 DEDENT
paren : /LPAREN (expr4|dent) /RPAREN
brace : /LCURLY (expr4|dent) /RCURLY 
brakt : /LBRAKT (expr4|dent) /RBRAKT
undent : /BACKSLASH dent

keyword : ID COLON
label : COLON ID?
-id : DASH ID
prop : expr0? DOT DASH? ID
op : (OP | DASH)+
