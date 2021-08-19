#lang br
(require brag/support syntax/readerr)

(define-lex-abbrevs
  (alpha (:+ alphabetic))
  (alnum (:+ (:or alphabetic numeric)))
  (digits (:+ (char-set "0123456789")))
  (newline (:seq spacetabs? (:or "\r\n" "\n")))
  (nextloc (:seq (:+ newline) (:* #\tab)))
  (quoted "\"")
  (quotes "'")
  (semi (:+ (:seq spacetabs? ";" spacetabs?)))
  (spacetabs? (:* (char-set " \t"))))

(define main-lexer
  (lexer-srcloc
   [semi token-NEWLINE]
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
   [(eof) (cond [(> current-indent-level 0) (token-DEDENT)])]
   [(:or (:+ #\space) #\tab) (token 'SPACE lexeme)]
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
   [",:" (token 'COMMA-COLON 'piped-to:)]
   ["," (token 'COMMA lexeme)]
   [":" (token 'COLON ':)]
   ["." (token 'DOT lexeme)]
   [(:+ (char-set "+-*/=><?`~@$%^&|"))      (token 'OP (string->symbol lexeme))]
   [(:seq alpha (:* (:seq (:* "-") alnum))) (token 'ID (string->symbol lexeme))]
   [(:seq (:? "-") digits "." digits)       (token 'DECIMAL (string->number lexeme))]
   [(:seq (:? "-") digits)                  (token 'INTEGER (string->number lexeme))]
   [(:or (:seq quotes nextloc quotes)
         (:seq quoted nextloc quoted)) (blank-string)]
   [(:seq quotes nextloc) (multiline-string quotes)]
   [(:seq quoted nextloc) (multiline-string quoted)]
   [quotes (str quotes)]
   [quoted (str quoted)]))

(define-macro (string-lexer QUOTCHAR CHARS ON-NEWLINE END-RULE)
  #'(lexer-srcloc ["#{" (list (token-LBRACE) (token 'BBRACE 'brace))]
                  ["#" (token 'STRING lexeme)]
                  [(:+ CHARS) (token 'STRING lexeme)]
                  [nextloc ON-NEWLINE]
                  END-RULE))

(define-macro (str QUOTCHAR)
  #'(token-QUOTE
     (string-lexer QUOTCHAR
                   (:~ QUOTCHAR "#" "\n")
                   (raise-read-error "unterminated string"
                                     (file-path) 
                                     (position-line start-pos)
                                     (position-col start-pos)
                                     (position-offset start-pos)
                                     (+ 0 (string-length lexeme)))
                   [QUOTCHAR (token-UNQUOTE)])))

(define-macro (multiline-string QUOTCHAR)
  #'(let [(extra-lines (expect-indent (add1 current-indent-level)))
          (multiline-string-lexer 
           (string-lexer QUOTCHAR
                         (:~ "#" "\n")
                         (cons token-NEWLINE (expect-indent current-indent-level))
                         [(:seq nextloc QUOTCHAR)
                          (let [(extra-lines (expect-indent current-indent-level))]
                            (append extra-lines (list (token-DEDENT) (token-UNQUOTE))))]))]
      (append (list (token-QUOTE multiline-string-lexer) (token-INDENT)) extra-lines)))

(define-macro (blank-string)
  #'(append (list (token 'QUOTE ''QUOTE))
            (expect-indent (add1 current-indent-level))
            (list (token 'UNQUOTE ''UNQUOTE))))

(define (calc-indent lexeme)
  (define last-line (last (string-split lexeme "\n" #:trim? #f)))
  (if (equal? (string-trim last-line) ";")
      current-indent-level
      (string-length last-line)))

(define-macro (expect-indent EXPECTED-LEVEL)
  #'(let [(start-line (position-line start-pos))
          (end-line (position-line end-pos))
          (indent-amount (calc-indent lexeme))]
      (if (= indent-amount EXPECTED-LEVEL)
          (make-list (sub1 (- end-line start-line)) token-NEWLINE)
          (raise-read-error "wrong indentation level"
                            (file-path) 
                            end-line
                            1
                            (- (position-offset end-pos) indent-amount)
                            indent-amount))))

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

(define (token-QUOTE lexer)
  (push-mode! lexer)
  (token 'QUOTE ''QUOTE))

(define (token-UNQUOTE)
  (pop-mode!)
  (token 'UNQUOTE ''UNQUOTE))

(define token-NEWLINE
  (token 'NEWLINE "\n"))

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
