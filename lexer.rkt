#lang br
(require brag/support syntax/readerr)

(define-lex-abbrevs
  (alpha (:+ alphabetic))
  (alnum (:+ (:or alphabetic numeric)))
  (comment (:seq "#!" spacetabs (:+ (:~ newline-char))))
  (digits (:+ (char-set "0123456789")))
  (identifier (:seq alpha (:* (:seq (:* "-") alnum))))
  (newline-char (char-set "\r\n"))
  (newline (:seq spacetabs? (:or "\r\n" "\n")))
  (nextloc (:seq (:+ newline) (:* #\tab)))
  (operator (:+ (char-set "+-*/=><?&|`~@$%^_#!")))
  (quoted "\"")
  (quotes "'")
  (semi (:+ (:seq spacetabs? ";" spacetabs?)))
  (spacetabs (:+ (:or #\space #\tab)))
  (spacetabs? (:* (:or #\space #\tab))))

(define main-lexer
  (lexer-srcloc
   [(:+ (:or nextloc semi))
    (let* ([next-level (add1 current-indent-level)]
           [indent-amount (calc-indent lexeme)]
           [excess-amount (- indent-amount next-level)])
      (cond
        [(= indent-amount current-indent-level) token-NEWLINE]
        [(> indent-amount current-indent-level)
         (cond [(= indent-amount next-level) (token-INDENT)]
               [else (raise-read-error "too much indentation"
                                       (file-path) 
                                       (position-line end-pos)
                                       next-level
                                       (- (position-offset end-pos) excess-amount)
                                       excess-amount)])]
        [(< indent-amount current-indent-level)
         (append (build-list (- current-indent-level indent-amount)
                             (lambda _ (token-DEDENT)))
                 (list token-NEWLINE))]))]
   [semi token-NEWLINE]
   [comment token-COMMENT]
   [identifier (token 'ID (string->symbol lexeme))]
   [operator (cdr (foldr (lambda (op lst)
                           (cons (token 'HASH-BANG "#!") 
                                 (if (non-empty-string? op)
                                     (cons (token 'OP (string->symbol op)) lst)
                                     lst)))
                         '()
                         (string-split lexeme "#!" #:trim? #f)))]
   [(:or ".." "..." "....") (token 'OP (string->symbol lexeme))]
   [(:or (:+ #\space) (:+ #\tab)) (token 'SPACE lexeme)]
   ["#!{" (cons (token 'MACRO lexeme) (token-LBRACE))]
   ["(" (token-LPAREN)]
   ["{" (token-LBRACE)]
   ["[" token-LBRACKET]
   [")" (token-RPAREN)]
   ["}" (token-RBRACE)]
   ["]" token-RBRACKET]
   ["\\" (token 'BACKSLASH lexeme)]
   [",:" (token 'PIPE 'piped-to:)]
   ["," (token 'COMMA lexeme)]
   [":" (token 'COLON ':)]
   ["." (token 'DOT lexeme)]
   [(:seq (:? "-") digits "." digits) (token 'DECIMAL (string->number lexeme))]
   [(:seq (:? "-") digits)            (token 'INTEGER (string->number lexeme))]
   [(:seq quotes nextloc) (multi-line-string quotes)]
   [(:seq quoted nextloc) (multi-line-string quoted)]
   [quotes string-quotes]
   [quoted (single-line-string quoted)]
   [(eof) (close-indents)]))

(define-macro token-COMMENT
  #'(token 'COMMENT lexeme))

(define-macro string-quotes
  #'(token-QUOTE (lexer-srcloc [newline-char (list (token-UNQUOTE) token-NEWLINE)]
                               ["#!{" (token-LBRACE)]
                               [(:seq "#!" spacetabs) token-COMMENT]
                               [(:+ (:~ "#" newline-char)) (token 'STRING lexeme)]
                               [any-char (token 'STRING lexeme)]
                               [(eof) (close-indents)])))

(define-macro (string-lexer QUOTCHAR CUSTOM-RULES ...)
  #'(lexer-srcloc CUSTOM-RULES ...
                  ["#!{" (token-LBRACE)]
                  [(:+ (:~ "#" newline-char QUOTCHAR)) (token 'STRING lexeme)]
                  [any-char (token 'STRING lexeme)]
                  [(eof) (unterminated-string)]))

(define block-lexer
  (string-lexer #\null
                [nextloc (append (cons token-NEWLINE (keep-indent))
                                 (append-if (equal? active-lexer main-lexer)
                                            token-NEWLINE))]
                [(eof) (close-indents)]))

(define-macro (single-line-string QUOTCHAR)
  #'(token-QUOTE (string-lexer QUOTCHAR
                               [newline-char (unterminated-string)]
                               [QUOTCHAR (token-UNQUOTE)])))

(define-macro (multi-line-string QUOTCHAR)
  #'(let* ([unquote-lexer 
            (lexer-srcloc [QUOTCHAR (token-UNQUOTE)]
                          [(:+ #\space) (raise-read-error "expected tabs, found spaces"
                                                          (file-path)
                                                          (position-line start-pos)
                                                          (position-col start-pos)
                                                          (position-offset start-pos)
                                                          (string-length lexeme))]
                          [any-char (unterminated-string)]
                          [(eof) (unterminated-string)])]
           [newline-on (lambda (tokens)
                         (if (equal? active-lexer unquote-lexer)
                             tokens
                             (cons token-NEWLINE tokens)))])
      (push-mode! unquote-lexer)
      (append (list (token-QUOTE (string-lexer QUOTCHAR
                                               [nextloc (newline-on (keep-indent))]))
                    (token-INDENT))
              (keep-indent))))

(define-macro (unterminated-string)
  #'(raise-read-error "unterminated string (missing closing quote)"
                      (file-path) 
                      (position-line start-pos)
                      (position-col start-pos)
                      (position-offset start-pos)
                      (if (string? lexeme) (string-length lexeme) 0)))

(define-macro (concat-if CONDITION LIST)
  #'(if CONDITION LIST '()))

(define-macro (append-if CONDITION ITEM)
  #'(if CONDITION (list ITEM) '()))

(define-macro (keep-indent)
  #'(let* ([tabs (regexp-match (pregexp (format "^\t{~a}(\t+)" current-indent-level))
                               (lastline lexeme))]
           [indent-amount (calc-indent lexeme)])
      (append (make-list (sub1 (- (position-line end-pos) (position-line start-pos)))
                         token-NEWLINE)
              (append-if tabs (token 'STRING (cadr tabs)))
              (concat-if (< indent-amount current-indent-level)
                         (begin (pop-mode!)
                                (build-list (- current-indent-level indent-amount)
                                            (lambda _ (token-DEDENT))))))))

(define (calc-indent lexeme)
  (define last-line (lastline lexeme))
  (if (string-contains? last-line ";")
      current-indent-level
      (string-length last-line)))

(define (close-indents)
  (cond [(> current-indent-level 0) 
         (build-list current-indent-level (lambda _ (token-DEDENT)))]))

(define (lastline lexeme)
  (last (string-split lexeme "\n" #:trim? #f)))

(define (rewind! port lexeme)
  (file-position port (- (file-position port) (string-length lexeme))))

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

(define token-NEWLINE
  (token 'NEWLINE "\n"))

(define (token-LPAREN)
  (push-mode! main-lexer)
  (list (token 'LPAREN ''LPAREN)
        (token 'BPAREN 'paren)))

(define (token-LBRACE)
  (push-mode! main-lexer)
  (list (token 'LBRACE ''LBRACE)
        (token 'BBRACE 'brace)))

(define token-LBRACKET
  (list (token 'LBRACKET ''LBRACKET)
        (token 'BBRACKET 'bracket)))

(define token-RBRACKET
  (token 'RBRACKET ''RBRACKET))

(define (token-RBRACE)
  (pop-mode!)
  (token 'RBRACE ''RBRACE))

(define (token-RPAREN)
  (pop-mode!)
  (token 'RPAREN ''RPAREN))

(define (token-QUOTE lexer)
  (begin
    (push-mode! lexer)
    (token 'QUOTE ''QUOTE)))

(define (token-UNQUOTE)
  (pop-mode!)
  (token 'UNQUOTE ''UNQUOTE))

(define pending-tokens '())
(define last-token void)
(define (toktype tok)
  (token-struct-type (srcloc-token-token tok)))

(define (bilang-lexer ip)
  (set! last-token 
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
  last-token)

(provide bilang-lexer)
