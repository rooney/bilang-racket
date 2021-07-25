#lang br
(require brag/support syntax/readerr)

(define-lex-abbrevs
  (alpha (:+ alphabetic))
  (alnum (:+ (:or alphabetic numeric)))
  (digits (:+ (char-set "0123456789"))))

(define current-level 0) ; current indentation level

(define (token-INDENT)
  (set! current-level (+ current-level 1))  
  (token 'INDENT ''INDENT))

(define (token-DEDENT)
  (set! current-level (- current-level 1))  
  (token 'DEDENT ''DEDENT))

(define bilang-lexer
  (lexer-srcloc
   [(:seq (:* #\space) "\n" (:* (char-set " \n")))
    (let* ([next-level (+ current-level 1)]
           [indent-amount (position-col end-pos)]
           [excess-amount (- indent-amount next-level)])
      (cond
        [(= indent-amount current-level) (token 'NEWLINE lexeme)]
        [(> indent-amount current-level)
         (cond [(= indent-amount next-level) (token-INDENT)]
               [else (raise-read-error "too much indentation"
                                       (file-path) 
                                       (position-line end-pos)
                                       next-level
                                       (- (position-offset end-pos) excess-amount)
                                       excess-amount
                                       )])]
        [(< indent-amount current-level)
         (cond [(< indent-amount current-level) ; need multiple dedents
                ; rewind the input so on next iteration it will produce dedent again
                (file-position input-port (- (file-position input-port) (string-length lexeme)))])
         (token-DEDENT)]))]
   [(eof) 
    (cond [(> current-level 0) (token-DEDENT)])]
   [#\space (token 'SPACE lexeme)]
   ["(" (token 'LPAREN lexeme)]
   [")" (token 'RPAREN lexeme)]
   ["[" (token 'LBRAKT lexeme)]
   ["]" (token 'RBRAKT lexeme)]
   ["{" (token 'LCURLY lexeme)]
   ["}" (token 'RCURLY lexeme)]
   ["\\" (token 'BACKSLASH lexeme)]
   ["." (token 'DOT lexeme)]
   [":" (token 'COLON ':)]
   [(:+ (char-set "+*/=><")) (token 'OP (string->symbol lexeme))]
   [(:+ "-") (token 'DASH (string->symbol lexeme))]
   [(:seq alpha (:* (:seq (:* "-") alnum)))
    (token 'ID (string->symbol lexeme))]
   [(:seq (:? "-") digits "." digits) (token 'DECIMAL (string->number lexeme))]
   [(:seq (:? "-") digits) (token 'INTEGER (string->number lexeme))]
   [(:or (from/to "\"" "\"") (from/to "'" "'"))
    (token 'STRING
           (substring lexeme
                      1 (sub1 (string-length lexeme))))]))

(provide bilang-lexer)
