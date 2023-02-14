#lang brag

expres : /feeds? expr4
@expr4 : expr3 /(SPACE|feeds|pseudent)*
@expr3 : apply3|comma3|semic3|break3
       | applyE|macro3|macro2|macro1
       | exprZ
@exprZ : applyZ
       | break2|breakD|breakK|break1|breakQ|break0
       | expr2
@expr2 : apply2|applyD
       |        commaD
       |        semicD
       | exprL
@exprL : semicK|semicQ|semic0|semic1|semic
       | exprK
@exprK : applyK
       | commaK|commaQ|comma0|comma1|comma
       | expr1
@expr1 : apply1
       | exprQ
@exprQ : applyQ
       | exprC
@exprC : groupQ
       | symbol
       | exprG
@exprG : applyG
       | group
       | group0
       | expr0
@expr0 : apply0
       | self
       | prop
       | e

applyE : exprZ /feeds expr3
       | exprZ /LINEFEED kv3
applyZ : exprZ /LINEFEED (kvZ|kvD|kv0)+

@macro : OP (/SPACE kv0)*
@mR    : (exprK|macro1|kv1)
@mL    : (expr1|
          comma|comma1|commaQ|comma0|
          semic|semic1|semicQ|semic0|
          break|break1|breakQ|break0) /SPACE

macro1 : mL  macro
       | mL? macro /SPACE mR
macro2 : mL? macro (/SPACE (mR denty |applyD|commaD|kvD) | denty)
macroD :     macro (/SPACE (mR denty |applyD|commaD|kvD) | denty)
macro3 : mL? macro (/SPACE (mR denty?|applyD|commaD|kvD) | denty)? /LINEFEED expr3
       | mL? macro /SPACE            (apply3|comma3|kv3)

@comma : exprK          /SPACE? /COMMA
@semic : (exprL|macro1) /SPACE? /SEMICOLON
@dents : (break2|              apply2|macro2|
          breakD|semicD|commaD|applyD|applyZ)    /LINEFEED /COMMA
@break : (exprZ|macro1|macro2)                   /LINEFEED /COMMA

break3 : (break|break0|breakQ)  /SPACE (kv3|apply3|comma3) | break kv3
break2 : (dents              )                        denty
       | (      break0|breakQ) (/SPACE (kv2|apply2) | denty)
       | (break              )  /SPACE (kv2|apply2)        | break kv2
breakD : (break|break0|breakQ)  /SPACE (kvD|applyD|commaD) | break kvD
breakK : (break|break0|breakQ)  /SPACE (kv1|applyK|commaK) | break kv1
break1 : (break|break0|breakQ)  /SPACE expr1
breakQ : (break|break0|breakQ) (/SPACE kv0)+               | break kv0
break0 :                                                     break (exprC|props)

semic3 : (semic|semic0|semicQ)  /SPACE (kv3|apply3|comma3) | semic kv3
semicD : (semic|semic0|semicQ)  /SPACE (kvD|applyD|commaD) | semic kvD
semicK : (semic|semic0|semicQ)  /SPACE (kv1|applyK|commaK) | semic kv1
semic1 : (semic|semic0|semicQ)  /SPACE exprK
semicQ : (semic|semic0|semicQ) (/SPACE  kv0)+              | semic kv0
semic0 :                                                     semic (exprC|props)

comma3 : (comma|comma0|commaQ)  /SPACE (kv3|apply3)        | comma kv3
commaD : (comma|comma0|commaQ)  /SPACE (kvD|applyD)        | comma kvD
commaK : (comma|comma0|commaQ)  /SPACE (kv1|applyK)        | comma kv1
comma1 : (comma|comma0|commaQ)  /SPACE expr1
commaQ : (comma|comma0|commaQ) (/SPACE  kv0)+              | comma kv0
comma0 :                                                     comma (exprC|props)

apply3 : exprQ  /SPACE (apply3|kv3) | (applyG|group) kv3
apply2 : exprL denty
applyD : exprQ  /SPACE (applyD|kvD) | (applyG|group) kvD
applyK : exprQ  /SPACE (applyK|kv1) | (applyG|group) kv1
apply1 : exprQ  /SPACE (expr1|OP /COMMA)
applyQ : exprC (/SPACE kv0)+
groupQ :                              (applyG|group) kv0
applyG : (exprG|OP) group
group0 :                              (applyG|group) (expr0|props)
apply0 : (expr0|OP) props
       | OP e
@e     : int|dec|ID|timed
timed  : (int|dec) ID

int    : INTEGER
dec    : DECIMAL
@kv0   : key (exprC|OP)
@kv1   : key /SPACE (macro1|exprK)
@kv2   : key /SPACE (macro2|apply2)
@kvZ   : key /SPACE (macro1|macro2|expr2)
@kv3   : key /feeds expr3
@kvD   : key (/SPACE macroD|dent)
key    :         (@int|@dec|OP|ID|DOT)+   /COLON
symbol : (/COLON (@int|@dec|OP|ID|DOT)*)? /COLON OP? ID? (dot|OP)*
dot    : /DOT (OP|OP? ID)
prop   : @dot (bind|paren)?
props  : (@prop|OP)+
bind   : /LPAREN kv0? skv0* bind1* bindX? /RPAREN
@skv0  : /SPACE kv0
bind1  : /SPACE? /COMMA /SPACE expr1
       | /SPACE? /COMMA        (kv0|exprQ|skv0) skv0*
bindX  : /SPACE? /COMMA        ((kv0|exprQ|skv0)? skv0* /SPACE)? (kv1|kv2|kvZ|kvD)
       | /SPACE? /COMMA /SPACE (applyK|applyD|apply2)
self   : /SELF

@group  : paren | brace | bracket | string
@paren  : /LPAREN (/SPACE? expr4|@dent /feeds|OP) /RPAREN
brace   : /LBRACE (/SPACE? expr4|@dent /feeds) /RBRACE
bracket : /LBRACKET (list|tuple) /RBRACKET
string  : /QUOTE /INDENT (STRING|interp|LINEFEED)* /DEDENT /UNQUOTE
        | /QUOTE         (STRING|interp)*                  /UNQUOTE
interp  : INTERPOLATE (brace|dent)

@denty   : dent | blockey
dent     : /INDENT expr4 /DEDENT
blockey  : /INDENT (kvZ|kvD|kv0) (/feeds (kvZ|kvD|kv0))* /feeds? /DEDENT
pseudent : /INDENT pseudent? /DEDENT

@ssc  : exprC (/SPACE exprC)*
@cs1  : /COMMA /SPACE expr1
@sz   : /SPACE (macro1|macro2|expr2) 
tuple : /SPACE? ssc (/feeds ssc)* /SPACE?
      | /INDENT ssc (/feeds ssc)* /DEDENT /LINEFEED
list  : /INDENT (/COMMA (sz|dent) /LINEFEED)* (/COMMA sz)? /DEDENT /LINEFEED
      | /feeds?
      | cs1+ (/LINEFEED cs1+)* /SPACE?

feeds : /LINEFEED+ | /BLANKLINE
