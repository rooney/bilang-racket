#lang br
(require brag/support syntax/readerr)

(define-lex-abbrevs
  (alpha (:+ alphabetic))
  (alnum (:+ (:or alphabetic numeric)))
  (digits (:+ (char-set "0123456789"))))

(define current-level 0) ; current indentation level

(define (token-INDENT)
  (set! current-level (add1 current-level))  
  (token 'INDENT ''INDENT))

(define (token-DEDENT)
  (set! current-level (sub1 current-level))  
  (token 'DEDENT ''DEDENT))

(define pending-tokens '())
(define (multi-token prod input-port lexeme)
  (cond [(empty? pending-tokens)
         (set! pending-tokens 
               (map (lambda (x)
                      (cond [(symbol? x) (token x lexeme)]
                            [(pair? x) (token (car x) (cdr x))]))
                    prod))])
  (cond [(> (length pending-tokens) 1)
         (rewind! input-port lexeme)])
  (pop! pending-tokens))

(define (rewind! input-port lexeme)
  (file-position input-port (- (file-position input-port) (string-length lexeme))))

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
            (rewind! input-port lexeme)
            (token-DEDENT)]))]

   [(eof) 
    (cond [(> current-level 0) (token-DEDENT)])]

   [#\space (token 'SPACE lexeme)]
   ["(" (multi-token '(LPAREN (PAREN . paren)) input-port lexeme)]
   [")" (token 'RPAREN lexeme)]
   ["{" (multi-token '(LBRACE (BRACE . brace)) input-port lexeme)]
   ["}" (token 'RBRACE lexeme)]
   ["[" (multi-token '(LBRACKET (BRACKET . bracket)) input-port lexeme)]
   ["]" (token 'RBRACKET lexeme)]
   ["\\" (token 'BACKSLASH lexeme)]
   [",:" (token 'PIPE lexeme)]
   ["," (token 'COMMA lexeme)]
   [":" (token 'COLON ':)]
   ["." (token 'DOT lexeme)]
   [(:+ (char-set "+-*/=><?")) (token 'OP (string->symbol lexeme))]
   [(:seq alpha (:* (:seq (:* "-") alnum)))
    (token 'ID (string->symbol lexeme))]
   [(:seq (:? "-") digits "." digits) (token 'DECIMAL (string->number lexeme))]
   [(:seq (:? "-") digits) (token 'INTEGER (string->number lexeme))]
   [(:or (from/to "\"" "\"") (from/to "'" "'"))
    (token 'STRING
           (substring lexeme
                      1 (sub1 (string-length lexeme))))]))

(provide bilang-lexer)
