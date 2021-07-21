#lang br
(require brag/support syntax/readerr)

(define-lex-abbrev digits (:+ (char-set "0123456789")))

(define current-level 0) ; current indentation level

(define (token-INDENT lexeme)
  (set! current-level (+ current-level 1))  
  (token 'INDENT lexeme))

(define (token-DEDENT lexeme)
  (set! current-level (- current-level 1))  
  (token 'DEDENT lexeme))

(define bilang-lexer
  (lexer-srcloc
   [(:seq (:* #\space) "\n" (:* (char-set " \n")))
    (let* ([next-level (+ current-level 1)]
           [indent-amount (position-col end-pos)]
           [excess-amount (- indent-amount next-level)])
      (cond
        [(= indent-amount current-level) (token 'NEWLINE lexeme)]
        [(> indent-amount current-level)
         (cond [(= indent-amount next-level) (token-INDENT lexeme)]
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
         (token-DEDENT lexeme)]))]
   [(eof) 
    (cond [(> current-level 0) (token-DEDENT lexeme)])]
   [#\space (token 'SPACE lexeme)]
   ["(" (token 'LPAREN lexeme)]
   [")" (token 'RPAREN lexeme)]
   ["[" (token 'LBRACKET lexeme)]
   ["]" (token 'RBRACKET lexeme)]
   ["{" (token 'LCURLY lexeme)]
   ["}" (token 'RCURLY lexeme)]
   [(:seq alphabetic (:* (:or alphabetic numeric)))
    (token 'ID (string->symbol lexeme))]
   [(:seq digits "." digits) (token 'DECIMAL (string->number lexeme))]
   [digits (token 'INTEGER (string->number lexeme))]
   [(:or (from/to "\"" "\"") (from/to "'" "'"))
    (token 'STRING
           (substring lexeme
                      1 (sub1 (string-length lexeme))))]))

(provide bilang-lexer)
