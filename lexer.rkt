#lang br
(require brag/support syntax/readerr)

(define-lex-abbrevs
  (digit (char-set "0123456789"))
  (digits (:+ digit))
  (hex (:or digit (char-set "abcdefABCDEF")))
  (alnums (:+ (:or alphabetic numeric)))
  (identifier (:seq alphabetic (:? alnums)
                    (:* (:seq (:or "-" "--" "/" "/-")
                              alnums))))
  (newline-char (char-set "\r\n"))
  (newline (:seq (:? spacetabs) (:or "\r\n" "\n")))
  (nextloc (:seq (:+ newline) (:* #\tab)))
  (operator (:+ (:or (char-set "+*/\\-~=><?!&|^#%$@") ".." "..."
                     (char-set "±÷√∫∂¬≈≠≥≤¿¡«»‹›‰§®©¢€£¥™°∞·…„"))))
  (d-quote #\")
  (s-quote #\')
  (b-quote #\`)
  (spacetabs (:+ (:or #\space #\tab))))

(define main-lexer
  (lexer-srcloc
   [nextloc (let ([next-level (add1 _level)]
                  [dent (measure-dent!)])
              (cond
                [(> dent next-level) (indentation-error next-level)]
                [(= dent next-level) (indent!)]
                [(= dent _level) (token-NEWLINE)]
                [(< dent _level) (cap-level! dent)]))]
   [spacetabs (token 'SPACE lexeme)]
   [(:seq (:? #\-) digits) (token 'INTEGER (string->number lexeme))]
   [(:seq (:? #\-) digits #\. digits) (token 'DECIMAL (string->number lexeme))]
   [(:seq d-quote nextloc) d-block]
   [(:seq s-quote nextloc) s-block]
   [(:seq b-quote nextloc) b-block]
   [(:seq s-quote #\{) s-brace]
   [(:seq b-quote #\{) b-brace]
   [d-quote d-str]
   [s-quote s-str]
   [b-quote b-str]
   [identifier (token 'ID (string->symbol lexeme))]
   [operator (token 'OP (string->symbol lexeme))]
   [#\( token-LPAREN]
   [#\) token-RPAREN]
   [#\[ token-LBRACK]
   [#\] token-RBRACK]
   [#\{ (token-LBRACE!)]
   [#\} (token-RBRACE!)]
   [#\: (token 'COLON ':)]
   [#\. (token 'DOT lexeme)]
   [#\, (token 'COMMA lexeme)]
   [(:+ #\,) (rr-error (string-append "Unexpected " lexeme))]
   [(eof) (if (> _level 0)
              (cap-level! 0) 
              (void))]))

(define-macro (strlex (CUSTOM-CHARS ...) CUSTOM-RULES ...)
  #'(lexer-srcloc CUSTOM-RULES ...
                  [(:+ (:~ newline-char CUSTOM-CHARS ...)) (token 'STRING lexeme)]
                  [any-char (token 'STRING lexeme)]))

(define-macro (strlex+ (CUSTOM-CHARS ...) CUSTOM-RULES ...)
  #'(strlex (b-quote #\\ CUSTOM-CHARS ...)
            CUSTOM-RULES ...
            [(:seq b-quote #\{) (begin (save-level!) (token-LBRACE!))]
            [(:seq "\\u" (:** 0 4 hex)) (unicode-escape 4)]
            [(:seq "\\U" (:** 0 8 hex)) (unicode-escape 8)]
            [(:seq #\\ any-char)
             (let ([char (hash-ref eschar (substring lexeme 1) #f)])
               (if char (token 'STRING char)
                   unknown-escape))]
            [#\\ unknown-escape]))

(define-macro (bracelex BASE)
  #'(BASE (#\{ #\})
          [#\{ (token-QUOTE! _mode)]
          [#\} (token-UNQUOTE!)]
          [newline-char unterminated-string]
          [(eof) unterminated-string]))

(define-macro (strblox CUSTOM-RULES ...)
  #'(strlex+ ()
             CUSTOM-RULES ...
             [(:seq (:? #\\) nextloc) (extract-whites)]
             [(:seq b-quote nextloc) str-interp]))

(define-macro d-str
  #'(token-QUOTE! (strlex+ (d-quote)
                           [d-quote (token-UNQUOTE!)]
                           [newline-char unterminated-string]
                           [(eof) unterminated-string])))

(define-macro s-str
  #'(token-QUOTE! (strlex (#\space #\tab #\,)
                          [(:or #\space #\tab #\, newline-char) (begin rewind! (token-UNQUOTE!))]
                          [(eof) (cons (token-UNQUOTE!)
                                       (cap-level! 0))])))

(define-macro b-str
  #'(token-QUOTE! (strlex+ ()
                           [newline-char (begin rewind! (token-UNQUOTE!))]
                           [(eof) (cons (token-UNQUOTE!)
                                        (cap-level! 0))])))

(define-macro s-brace
  #'(token-QUOTE! (bracelex strlex)))

(define-macro b-brace
  #'(token-QUOTE! (bracelex strlex+)))

(define-macro d-block
  #'(append (list (token-QUOTE! (lexer-srcloc [d-quote (token-UNQUOTE!)]
                                              [any-char unterminated-string]
                                              [(eof) unterminated-string]))
                  (indent! (strblox [(eof) unterminated-string])))
            (-1LF (extract-whites))))

(define-macro s-block
  #'(append (list (token-QUOTE! 'DEDENT<-UNQUOTE)
                  (indent! (strlex ()
                                   [nextloc (extract-whites)]
                                   [(eof) (cap-level! 0)])))
            (-1LF (extract-whites))))

(define-macro b-block
  #'(append (list (token-QUOTE! 'DEDENT<-UNQUOTE)
                  (indent! (strblox [(eof) (cap-level! 0)])))
            (-1LF (extract-whites))))

(define-macro str-interp
  #'(let ([prev-level (save-level!)]
          [next-level (add1 _level)]
          [dent (measure-dent!)])
      (cond
        [(> dent next-level) (indentation-error next-level)]
        [(< dent next-level) (indentation-error)]
        [(= dent next-level) (list (token 'BQUOTE (- dent prev-level))
                                   (indent!))])))

(define-macro (concat-if COND LIST)
  #'(if COND LIST empty))

(define-macro (append-if COND ITEM)
  #'(if COND (list ITEM) empty))

(define-macro (starts-with? PREFIX STR)
  #'(and (not (equal? eof STR))
         (string-prefix? STR PREFIX)))

(define eschar #hash(("`" . "`")
                     ("n" . "\n")
                     ("r" . "\r")
                     ("t" . "\t")
                     ("\"" . "\"")
                     ("\\" . "\\")))

(define-macro (unicode-escape LENGTH)
  #'(if (< (string-length lexeme) (+ 2 LENGTH))
        (rr-error (string-append (substring lexeme 0 2) " must be followed by "
                                 (number->string LENGTH) " hex digits, got "
                                 (number->string (- (string-length lexeme) 2))))
        (token 'STRING (string (integer->char (string->number (substring lexeme 3) 16))))))

(define-macro unknown-escape
  #'(rr-error (string-append "Unknown escape sequence: " (if (string? lexeme) lexeme "EOF"))))

(define-macro unterminated-string
  #'(rr-error "Unterminated string (missing closing quote)"))

(define-macro (measure-dent!)
  #'(begin (set! _dent (string-length (last (string-split lexeme "\n" #:trim? #f))))
           _dent))

(define-macro-cases indentation-error
  [(indentation-error)     #'(indentation-error 1 _dent "Insufficient indentation")]
  [(indentation-error COL) #'(indentation-error COL (- _dent COL) "Too much indentation")]
  [(indentation-error COL LENGTH MSG)
   #'(rr-error MSG (position-line end-pos) COL 
               (- (position-offset end-pos) LENGTH) LENGTH)])

(define-macro-cases rr-error
  [(rr-error MSG LINE COL OFFSET LENGTH) #'(raise-read-error MSG (file-path) LINE COL OFFSET LENGTH)]
  [(rr-error MSG) #'(rr-error MSG
                              (position-line start-pos)
                              (position-col start-pos)
                              (position-offset start-pos)
                              (if (string? lexeme) (string-length lexeme) 0))])

(define (debug x)
  (println x) x)

(define-macro rewind!
  #'(file-position input-port (- (file-position input-port) (string-length lexeme))))

(define _mode main-lexer)
(define _suspends '())

(define (push-mode! lexer)
  (push! _suspends _mode)
  (set! _mode lexer))

(define (pop-mode!)
  (set! _mode (pop! _suspends))
  (if (jumpto? _mode)
      (begin (set! _level (jumpto-level _mode))
             (pop-mode!))
      void)
  _mode)

(struct jumpto (level))
(define _level 0)
(define _dent 0)

(define (indent! [lexer main-lexer])
  (set! _level (add1 _level))
  (push-mode! lexer)
  (token 'INDENT _mode))

(define (dedent!)
  (set! _level (sub1 _level))
  (pop-mode!)
  (append (list (token 'DEDENT _mode))
          (concat-if (equal? _mode 'DEDENT<-UNQUOTE) (list (token-UNQUOTE!)
                                                           (token-NEWLINE)))))

(define-macro (cap-level! CAP)
  #'(let* ([dedents '()]
           [_ (while (> _level CAP)
                     (set! dedents (append dedents (dedent!))))]
           [escaped? (starts-with? "\\" lexeme)]
           [num-feed (+ numLF
                        (if (empty? dedents) 0 -1)
                        (if escaped? -1 0))]
           [_ (and (< num-feed 0) escaped? 
                   (unknown-escape))])
      (append (concat-if (> num-feed 0)
                         (make-list num-feed (token-NEWLINE)))
              dedents
              (append-if (and (> _level 0)
                              (> _dent _level))
                         (token-STRING (- _dent _level) #\tab)))))

(define (save-level!)
  (define prev-level _level)
  (set! _level _dent)
  (push-mode! (jumpto prev-level))
  prev-level)

(define-macro extract-whites
  #'(cap-level! (min _level (measure-dent!))))

(define-macro -1LF
  #'(let ([whites extract-whites])
      (if (equal? 'NEWLINE (token-struct-type (car whites)))
          (cdr whites) whites)))

(define-macro numLF
  #'(- (position-line end-pos)
       (position-line start-pos)))

(define pending-tokens '())

(define (token-NEWLINE)
  (token 'NEWLINE "\n"))

(define (token-STRING count char)
  (token 'STRING (make-string count char)))

(define token-LPAREN
  (list (token 'LPAREN ''LPAREN)
        (token 'PAREN 'paren)))

(define token-RPAREN
  (token 'RPAREN ''RPAREN))

(define token-LBRACK
  (list (token 'LBRACK ''LBRACK)
        (token 'BRACKET 'bracket)))

(define token-RBRACK
  (token 'RBRACK ''RBRACK))

(define (token-LBRACE!)
  (push-mode! main-lexer)
  (list (token 'LBRACE _mode)
        (token 'BRACE 'brace)))

(define-macro token-RBRACE!
  #'(begin (cond [(empty? _suspends) (rr-error "No matching pair")])
           (pop-mode!)
           (token 'RBRACE _mode)))

(define (token-QUOTE! lexer)
  (push-mode! lexer)
  (token 'QUOTE "{"))

(define (token-UNQUOTE!)
  (pop-mode!)
  (token 'UNQUOTE "}"))

(define (bilang-lexer ip)
  (if (empty? pending-tokens)
      (let* ([produce (_mode ip)]
             [tokens (and (srcloc-token? produce) 
                          (srcloc-token-token produce))])
        (cond
          [(empty? tokens) (bilang-lexer ip)]
          [(list? tokens)
           (let ([prods (map (lambda (t)
                               (srcloc-token t 
                                             (srcloc-token-srcloc produce)))
                             tokens)])
             (set! pending-tokens (cdr prods))
             (car prods))]
          [else produce]))
      (pop! pending-tokens)))

(provide bilang-lexer)
