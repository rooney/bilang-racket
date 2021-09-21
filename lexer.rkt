#lang br
(require brag/support syntax/readerr)

(define-lex-abbrevs
  (alpha (:+ alphabetic))
  (alnum (:+ (:or alphabetic numeric)))
  (comment (:seq "#!" spacetabs (:+ (:~ newline-char))))
  (digits (:+ (char-set "0123456789")))
  (identifier (:seq alpha (:? alnum)))
  (newline-char (char-set "\r\n"))
  (newline (:seq spacetabs? (:or "\r\n" "\n")))
  (nextloc (:seq (:+ newline) (:* #\tab)))
  (operator (:+ (char-set "+-*/=><?&|`~@$%^_#!")))
  (quoted "\"")
  (quotes "'")
  (semi (:+ (:seq spacetabs? ";" spacetabs?)))
  (spacetabs (:+ (:or #\space #\tab)))
  (spacetabs? (:? spacetabs)))

(define main-lexer
  (lexer-srcloc
   [(:+ (:or nextloc semi))
    (measure-indent-amount ([next-level (add1 _indent-level)]
                            [excess-amount (- _indent-amount next-level)])
                           (cond
                             [(= _indent-amount _indent-level) token-NEWLINE]
                             [(> _indent-amount _indent-level)
                              (cond [(= _indent-amount next-level) (token-INDENT)]
                                    [else (rr-error "too much indentation"
                                                    (position-line end-pos)
                                                    next-level
                                                    (- (position-offset end-pos) excess-amount)
                                                    excess-amount)])]
                             [(< _indent-amount _indent-level)
                              (append (close-indents _indent-amount)
                                      (append-if (mode-is main-lexer)
                                                 token-NEWLINE))]))]
   [semi token-NEWLINE]
   [comment (begin (println last-token) (token 'COMMENT lexeme))]
   [identifier (token 'ID (string->symbol lexeme))]
   [operator (cdr (foldr (lambda (op ops)
                           (cons (token 'HASH-BANG "#!") 
                                 (if (non-empty-string? op)
                                     (cons (token 'OP (string->symbol op)) ops)
                                     ops)))
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
   [(:seq digits "." digits) (token 'DECIMAL (string->number lexeme))]
   [digits (token 'INTEGER (string->number lexeme))]
   [(:seq quotes nextloc) multi-quotes]
   [(:seq quoted nextloc) multi-quoted]
   [quotes string-quotes]
   [quoted string-quoted]
   [(eof) (if-applicable (close-indents))]))

(define-macro (string-lexer (CUSTOM-CHARS ...) CUSTOM-RULES ...)
  #'(lexer-srcloc CUSTOM-RULES ...
                  ["#!{" (token-LBRACE)]
                  [(:+ (:~ "#" newline-char CUSTOM-CHARS ...)) (token 'STRING lexeme)]
                  [any-char (token 'STRING lexeme)]))

(define-macro string-quotes
  #'(token-QUOTE (string-lexer ()
                               [newline-char (begin rewind! (token-UNQUOTE))]
                               [(eof) (cons (token-UNQUOTE)
                                            (close-indents))])))

(define-macro string-quoted
  #'(token-QUOTE (string-lexer (quoted)
                               [quoted (token-UNQUOTE)]
                               [newline-char (unterminated-string)]
                               [(eof) (unterminated-string)])))

(define-macro multi-quotes
  #'(append (list (token-QUOTE 'UNQUOTE-ON-DEDENT)
                  (token-INDENT (string-lexer ("`")
                                              [(:seq "`" nextloc) back-quote]
                                              [nextloc (keep-indent token-NEWLINE token-NEWLINE)]
                                              [(eof) (close-indents)])))
            (keep-indent)))

(define-macro multi-quoted
  #'(append (list (token-QUOTE (lexer-srcloc [quoted (token-UNQUOTE)]
                                             [(:+ #\space) (rr-error "expected tabs, found spaces")]
                                             [any-char (unterminated-string)]
                                             [(eof) (unterminated-string)]))
                  (token-INDENT (string-lexer ()
                                              [nextloc (keep-indent token-NEWLINE)]
                                              [(eof) (unterminated-string)])))
            (keep-indent)))

(define-macro back-quote
  #'(begin (push-mode! #;jump-to _indent-level)
           (set! _indent-level _indent-amount)
           (measure-indent-amount ()
             (if (= _indent-amount (add1 _indent-level))
                 (list (token 'BQUOTE ''BQUOTE) (token-INDENT))
                 (rr-error "wrong indentation level"
                           (position-line end-pos)
                           1
                           (- (position-offset end-pos) _indent-amount)
                           _indent-amount)))))

(define-macro (unterminated-string)
  #'(rr-error "unterminated string (missing closing quote)"))

(define-macro (concat-if COND LIST)
  #'(if COND LIST empty))

(define-macro (append-if COND ITEM)
  #'(if COND (enlist ITEM) empty))

(define-macro (append-unless COND ITEM)
  #'(if COND empty (enlist ITEM)))

(define (if-applicable x)
  (if (empty? x) (void) x))

(define (enlist x)
  (if x (list x) empty))

(define-macro-cases keep-indent
  [(keep-indent) #'(keep-indent #f #f)]
  [(keep-indent DENT-ON) #'(keep-indent DENT-ON #f)]
  [(keep-indent DENT-ON DENT-OFF)
   #'(measure-indent-amount
      ([tabs (regexp-match (pregexp (format "^\t{~a}(\t+)" _indent-level))
                           (lastline lexeme))]
       [end-dent? (< _indent-amount _indent-level)])
      (append (make-list (sub1 (- (position-line end-pos) (position-line start-pos)))
                         token-NEWLINE)
              (append-unless end-dent? DENT-ON)
              (append-if tabs (token 'STRING (cadr tabs)))
              (concat-if end-dent? (append (close-indents _indent-amount)
                                           (enlist DENT-OFF)))))])

(define-macro (measure-indent-amount DEFS BODY ...)
  #'(let ([last-line (lastline lexeme)])
      (set! _indent-amount (if (string-contains? last-line ";")
                               _indent-level
                               (string-length last-line)))
      (let* DEFS BODY ...)))

(define (close-indents [target-level 0])
  (define closers '())
  (while (> _indent-level target-level)
         (set! closers (append closers (dedent!))))
  closers)

(define (lastline lexeme)
  (last (string-split lexeme "\n" #:trim? #f)))

(define-macro rewind!
  #'(file-position input-port (- (file-position input-port) (string-length lexeme))))

(define-macro-cases rr-error
  [(rr-error MSG LINE COL OFFSET LENGTH) #'(raise-read-error MSG (file-path) LINE COL OFFSET LENGTH)]
  [(rr-error MSG) #'(rr-error MSG
                              (position-line start-pos)
                              (position-col start-pos)
                              (position-offset start-pos)
                              (if (string? lexeme) (string-length lexeme) 0))])

(define _mode main-lexer)
(define modes '())
(define (mode-is x)
  (equal? _mode x))

(define (push-mode! lexer)
  (push! modes _mode)
  (set! _mode lexer))

(define (pop-mode!)
  (set! _mode (pop! modes)))

(define _indent-level 0)
(define _indent-amount 0)

(define (token-INDENT [lexer main-lexer])
  (set! _indent-level (add1 _indent-level))
  (push-mode! lexer)
  (token 'INDENT _mode))

(define (dedent!)
  (pop-mode!)
  (define jump-to? (integer? _mode))
  (set! _indent-level (if jump-to?
                          (get-and-set! _mode (pop-mode!))
                          (sub1 _indent-level)))
  (append (list (token 'DEDENT _mode))
          (append-if (mode-is 'UNQUOTE-ON-DEDENT)
                     (token-UNQUOTE))
          (concat-if (and jump-to? (> _indent-amount _indent-level))
                     (list token-NEWLINE
                           (token 'STRING (make-string (- _indent-amount _indent-level)
                                                       #\tab))))))

(define (get-and-set! x . side-effects)
  x)

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

(define (token-QUOTE unquoter)
  (push-mode! unquoter)
  (token 'QUOTE _mode))

(define (token-UNQUOTE)
  (pop-mode!)
  (token 'UNQUOTE _mode))

(define pending-tokens '())
(define last-token #f)
(define (toktype t)
  (and t (token-struct-type (srcloc-token-token t))))

(define (bilang-lexer ip)
  (set! last-token 
        (if (empty? pending-tokens)
            (let* ([produce (_mode ip)]
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
