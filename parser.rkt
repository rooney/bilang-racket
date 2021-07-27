#lang brag
return : /NEWLINE? expr3

@expr3 : apply3
       | exprZ (/NEWLINE | /INDENT /DEDENT)?
@exprZ : applyZ
       | apply2
       | op
       | expr1
@expr1 : apply1
       | exprO
@exprO : applyO
       | keyword
       | expr0 
@expr0 : apply0
       | e

@e : INTEGER | DECIMAL
   | STRING 
   | label | ID
   | prop
   | paren | brace | bracket | comma | undent
   | PAREN | BRACE | BRACKET

apply3 : exprZ /NEWLINE expr3
applyZ : exprO /SPACE (applyZ|op)
       | op /SPACE exprZ
       | op dent
apply2 : expr1 dent
apply1 : exprO /SPACE expr1
applyO : keyword exprO
       | expr0 op
apply0 : expr0 e
       | op e

@comma : expr1 /COMMA
       | expr3 /NEWLINE /COMMA
@subexpr : expr3
         | dent /NEWLINE 
@dent : /INDENT expr3 /DEDENT
@undent : /BACKSLASH dent

@paren : /LPAREN subexpr /RPAREN
@brace : /LBRACE subexpr /RBRACE 
@bracket : /LBRACKET subexpr /RBRACKET

keyword : (OP | OP? ID OP?) COLON
label : COLON OP? ID? OP?
prop : DOT ID
@op : OP
