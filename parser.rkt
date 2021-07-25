#lang brag

return : /NEWLINE? expr3

@expr3 : apply3
       | expr2 (/NEWLINE | /INDENT /DEDENT)?

@expr2 : applyZ
       | apply2
       | expr1
       | op

@expr1 : apply1
       | exprO

@exprO : applyO
       | expr0 

@expr0 : apply0
       | e

@e : num
   | STRING
   | keyword | label | name
   | prop
   | grouping

apply3 : expr2 /NEWLINE expr3
applyZ : expr0 /SPACE (applyZ|op)
       | op /SPACE expr2
       | op dent
apply2 : expr1 dent
apply1 : exprO /SPACE expr1
applyO : expr0 op
apply0 : expr0 e
       | op e

@num : INTEGER | DECIMAL
@name : ID | -id
@grouping : paren | brace | brakt | undent

dent : /INDENT expr3 /DEDENT
paren : /LPAREN (expr2|dent) /RPAREN
brace : /LCURLY (expr2|dent) /RCURLY 
brakt : /LBRAKT (expr2|dent) /RBRAKT
undent : /BACKSLASH dent

keyword : ID COLON
label : COLON ID?
-id : DASH ID
prop : e? DOT DASH? ID
op : (OP | DASH)+
