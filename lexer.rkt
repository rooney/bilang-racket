#lang br
(require brag/support syntax/readerr)

(define-lex-abbrevs
  (alpha (:+ alphabetic))
  (alnum (:+ (:or alphabetic numeric)))
  (digits (:+ (char-set "0123456789")))
  (quotd "\"")
  (quots "'"))

(define main-lexer
  (lexer-srcloc
   [(:seq (:* #\space) "\n" (:* (char-set " \n")))
    (let* ([next-level (+ current-indent-level 1)]
           [indent-amount (position-col end-pos)]
           [excess-amount (- indent-amount next-level)])
      (cond
        [(= indent-amount current-indent-level) (token 'NEWLINE lexeme)]
        [(> indent-amount current-indent-level)
         (cond [(= indent-amount next-level) (token-INDENT)]
               [else (raise-read-error "too much indentation"
                                       (file-path) 
                                       (position-line end-pos)
                                       next-level
                                       (- (position-offset end-pos) excess-amount)
                                       excess-amount
                                       )])]
        [(< indent-amount current-indent-level)
         (append (build-list (- current-indent-level indent-amount)
                             (lambda _ (token-DEDENT))) 
                 (list (token 'NEWLINE lexeme)))]))]
   [(eof) 
    (cond [(> current-indent-level 0) (token-DEDENT)])]
   [#\space (token 'SPACE lexeme)]
   ["(" (list (token 'LPAREN lexeme)
              (token 'BPAREN 'paren))]
   ["[" (list (token 'LBRACKET lexeme)
              (token 'BBRACKET 'bracket))]
   ["{" (list (token-LBRACE)
              (token 'BBRACE 'brace))]
   [")" (token 'RPAREN lexeme)]
   ["]" (token 'RBRACKET lexeme)]
   ["}" (token-RBRACE)]
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
   [quots (token-QUOT lexeme quots-lexer)]
   [quotd (token-QUOT lexeme quotd-lexer)]))

(define-macro (quot-lexer QUOT-TYPE)
  #'(lexer-srcloc
       [(:+ (:~ QUOT-TYPE "#")) (token 'STRING lexeme)]
       ["#" (token 'STRING lexeme)]
       ["#{" (list (token-LBRACE)
                   (token 'BBRACE 'brace))]
       [QUOT-TYPE
        (begin
          (pop-mode!)
          (token 'UNQUOT lexeme))]))

(define quotd-lexer (quot-lexer quotd))
(define quots-lexer (quot-lexer quots))

(define active-lexer main-lexer)
(define modes '())

(define (push-mode! lexer)
  (push! modes active-lexer)
  (set! active-lexer lexer))

(define (pop-mode!)
  (set! active-lexer (pop! modes)))

(define current-indent-level 0)

(define (token-INDENT)
  (set! current-indent-level (add1 current-indent-level))  
  (token 'INDENT ''INDENT))

(define (token-DEDENT)
  (set! current-indent-level (sub1 current-indent-level))  
  (token 'DEDENT ''DEDENT))

(define (token-LBRACE)
  (push-mode! main-lexer)
  (token 'LBRACE ''LBRACE))

(define (token-RBRACE)
  (pop-mode!)
  (token 'RBRACE ''RBRACE))

(define (token-QUOT lexeme lexer)
  (push-mode! lexer)
  (token 'QUOT lexeme))

(define pending-tokens '())
(define (bilang-lexer ip)
  (if (empty? pending-tokens)
      (let* ([produce (active-lexer ip)]
             [tokens (and (srcloc-token? produce) 
                          (srcloc-token-token produce))])
        (if (list? tokens)
            (let* ([t-loc (srcloc-token-srcloc produce)]
                   [prods (map (lambda (t) (srcloc-token t t-loc))
                               tokens)])
              (set! pending-tokens (cdr prods))
              (car prods))
            produce))
      (pop! pending-tokens)))

(provide bilang-lexer)
