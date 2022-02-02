#lang br
(require brag/support syntax/readerr)

(define-lex-abbrevs
  (alnums (:+ (:or alphabetic numeric)))
  (digits (:+ (char-set "0123456789")))
  (identifier (:seq alphabetic 
                    (:? alnums)
                    (:* (:seq (char-set "-/") alnums))))
  (newline-char (char-set "\r\n"))
  (newline (:seq (:? spacetabs) (:or "\r\n" "\n")))
  (nextloc (:seq (:+ newline) (:* #\tab)))
  (operator (:+ (:or ".." "..." (char-set "+-*/=><?!#%@$&|\\"))))
  (quoted "\"")
  (quotes "'")
  (quoteb "`")
  (spacetabs (:+ (:or #\space #\tab))))

(define main-lexer
  (lexer-srcloc
   [nextloc (let ([numlines (- (position-line end-pos) (position-line start-pos))])
              (measure-dent! (lastline lexeme))
              (cond
                [(= _curdent _curlevel) token-NEWLINE]
                [(> _curdent _curlevel)
                 (let ([next-level (add1 _curlevel)])
                   (cond [(= _curdent next-level) (indent!)]
                         [else (let ([excess (- _curdent next-level)]) 
                                 (rr-error "too much indentation"
                                           (position-line end-pos)
                                           next-level
                                           (- (position-offset end-pos) excess)
                                           excess))]))]
                [(< _curdent _curlevel)
                 (append (unlevel! _curdent)
                         (enlist (if (mode-is main-lexer)
                                     token-NEWLINE
                                     (token-STRING (sub1 numlines) #\newline))))]))]
   [spacetabs (token 'SPACE lexeme)]
   [(:seq (:? "-") digits) (token 'INTEGER (string->number lexeme))]
   [(:seq (:? "-") digits "." digits) (token 'DECIMAL (string->number lexeme))]
   [(:seq quotes nextloc) multi-quotes]
   [(:seq quoted nextloc) multi-quoted]
   [quotes string-quotes]
   [quoted string-quoted]
   [identifier (token 'ID (string->symbol lexeme))]
   [operator (token 'OP (string->symbol lexeme))]
   [(:+ (:or ",," ",,,")) (rr-error (string-append "unexpected " lexeme))]
   ["," (token 'COMMA lexeme)]
   [":" (token 'COLON ':)]
   ["." (token 'DOT lexeme)]
   ["(" token-LPAREN]
   ["[" token-LBRACK]
   ["{" (token-LBRACE!)]
   [")" token-RPAREN]
   ["]" token-RBRACK]
   ["}" (token-RBRACE!)]
   [(eof) (void/unless (unlevel!))]))

(struct jumpto (level))

(define-macro (string-lexer (CUSTOM-CHARS ...) CUSTOM-RULES ...)
  #'(lexer-srcloc CUSTOM-RULES ...
                  ["#!{" (token-LBRACE!)]
                  [(:+ (:~ "#" newline-char CUSTOM-CHARS ...)) (token 'STRING lexeme)]
                  [any-char (token 'STRING lexeme)]))

(define-macro string-quotes
  #'(token-QUOTE! (string-lexer ()
                                [newline-char (begin rewind! (token-UNQUOTE!))]
                                [(eof) (cons (token-UNQUOTE!)
                                             (unlevel!))])))

(define-macro string-quoted
  #'(token-QUOTE! (string-lexer (quoted)
                                [quoted (token-UNQUOTE!)]
                                [newline-char (unterminated-string)]
                                [(eof) (unterminated-string)])))

(define-macro multi-quotes
  #'(append (list (token-QUOTE! 'DEDENT->UNQUOTE)
                  (indent! (string-lexer ("`")
                                         [(:seq "`" nextloc) back-quote]
                                         [nextloc (keep-level ? token-NEWLINE else token-NEWLINE)]
                                         [(eof) (unlevel!)])))
            (keep-level)))

(define-macro multi-quoted
  #'(append (list (token-QUOTE! (lexer-srcloc [quoted (token-UNQUOTE!)]
                                              [(:+ #\space) (rr-error "expected tabs, found spaces")]
                                              [any-char (unterminated-string)]
                                              [(eof) (unterminated-string)]))
                  (indent! (string-lexer ()
                                         [nextloc (keep-level ? token-NEWLINE)]
                                         [(eof) (unterminated-string)])))
            (keep-level)))

(define-macro back-quote
  #'(begin (push-mode! (jumpto _curlevel))
           (set! _curlevel _curdent)
           (measure-dent! (lastline lexeme))
           (if (= _curdent (add1 _curlevel))
               (list (token 'BQUOTE ''BQUOTE) (indent!))
               (rr-error "wrong indentation level"
                         (position-line end-pos)
                         1
                         (- (position-offset end-pos) _curdent)
                         _curdent))))

(define-macro (unterminated-string)
  #'(rr-error "unterminated string (missing closing quote)"))

(define-macro (concat-if COND LIST)
  #'(if COND LIST empty))

(define-macro (append-if COND ITEM)
  #'(if COND (enlist ITEM) empty))

(define-macro (append-unless COND ITEM)
  #'(if COND empty (enlist ITEM)))

(define (void/unless x)
  (if (empty? x) (void) x))

(define (enlist x)
  (if x (list x) empty))

(define-macro-cases keep-level
  [(keep-level) #'(keep-level ? #f)]
  [(keep-level ? DENT-ON) #'(keep-level ? DENT-ON else #f)]
  [(keep-level ? DENT-ON else DENT-OFF)
   #'(let ([last-line (lastline lexeme)])
       (measure-dent! last-line)
       (define stop? (< _curdent _curlevel))
       (define extra-tabs (regexp-match (pregexp (format "^\t{~a}(\t+)" _curlevel)) last-line))
       (append (make-list (sub1 (- (position-line end-pos) (position-line start-pos)))
                          token-NEWLINE)
               (append-unless stop? DENT-ON)
               (append-if extra-tabs (token 'STRING (cadr extra-tabs)))
               (concat-if stop? (append (unlevel! _curdent)
                                        (enlist DENT-OFF)))))])

(define (measure-dent! line)
  (set! _curdent (string-length line)))

(define (unlevel! [target-level 0])
  (define tokens '())
  (while (> _curlevel target-level)
         (set! tokens (append tokens (dedent!))))
  tokens)

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

(define modes '())
(define _mode main-lexer)

(define (mode-is x)
  (equal? _mode x))

(define (push-mode! lexer)
  (push! modes _mode)
  (set! _mode lexer))

(define (pop-mode!)
  (set! _mode (pop! modes)))

(define _curlevel 0)
(define _curdent 0)

(define (indent! [lexer main-lexer])
  (set! _curlevel (add1 _curlevel))
  (push-mode! lexer)
  (token 'INDENT _mode))

(define (dedent!)
  (pop-mode!)
  (define jump? (jumpto? _mode))
  (set! _curlevel (if jump?
                      (jumpto-level (get-current _mode #:then (pop-mode!)))
                      (sub1 _curlevel)))
  (append (list (token 'DEDENT _mode))
          (append-if (mode-is 'DEDENT->UNQUOTE)
                     (token-UNQUOTE!))
          (concat-if (and jump? (> _curdent _curlevel))
                     (list token-NEWLINE
                           (token-STRING (- _curdent _curlevel) #\tab)))))

(define (get-current x #:then side-effects)
  x)

(define token-NEWLINE
  (token 'NEWLINE "\n"))

(define (token-STRING count char)
  (and (> count 0)
       (token 'STRING (make-string count char))))

(define token-LPAREN
  (list (token 'LPAREN ''LPAREN)
        (token 'BPAREN 'paren)))

(define token-LBRACK
  (list (token 'LBRACK ''LBRACK)
        (token 'BBRACK 'bracket)))

(define (token-LBRACE!)
  (push-mode! main-lexer)
  (list (token 'LBRACE ''LBRACE)
        (token 'BBRACE 'brace)))

(define token-RPAREN
  (token 'RPAREN ''RPAREN))

(define token-RBRACK
  (token 'RBRACK ''RBRACK))

(define (token-RBRACE!)
  (pop-mode!)
  (token 'RBRACE ''RBRACE))

(define (token-QUOTE! unquoter)
  (push-mode! unquoter)
  (token 'QUOTE _mode))

(define (token-UNQUOTE!)
  (pop-mode!)
  (token 'UNQUOTE _mode))

(define pending-tokens '())
(define (bilang-lexer ip)
  (if (empty? pending-tokens)
      (let* ([produce (_mode ip)]
             [tokens (and (srcloc-token? produce) 
                          (srcloc-token-token produce))])
        (if (list? tokens)
            (let* ([prods (map (lambda (t) (srcloc-token t (srcloc-token-srcloc produce)))
                               tokens)])
              (set! pending-tokens (cdr prods))
              (car prods))
            produce))
      (pop! pending-tokens)))

(provide bilang-lexer)
