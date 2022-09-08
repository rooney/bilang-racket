#lang br
(require brag/support syntax/readerr)

(define-lex-abbrevs
  (digits (:+ (:/ #\0 #\9)))
  (number (:seq digits (:* (:seq #\_ digits))))
  (integer (:seq (:? #\-) number))
  (decimal (:seq integer #\. number))
  (alpha (:/ #\a #\z #\A #\Z))
  (alnum (:/ #\a #\z #\A #\Z #\0 #\9))
  (name (:seq alpha
              (:* alnum)
              (:* (:seq (:or "-" "+" "/" "<-") (:+ alnum)))))
  (identifier (:seq name prime?))
  (name/op (:or name operator))
  (key (:seq name/op (:* (:seq (:? #\.) name/op)) prime?))
  (arg (:seq key #\:))
  (param (:seq #\: (:? key)))
  (newline-char (char-set "\r\n"))
  (newline (:seq (:? spacetabs) (:or "\r\n" "\n")))
  (nextloc (:seq (:+ newline) (:* #\tab)))
  (operator (:+ (:or (char-set "+*/\\-~=><?!&|^#%$@") ".." "...")))
  (s-quote #\')
  (d-quote #\")
  (b-quote #\`)
  (prime? (:* s-quote))
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
   [decimal (token 'DECIMAL (string->number lexeme))]
   [integer (token 'INTEGER (string->number lexeme))]
   [(:seq s-quote nextloc) s-block]
   [(:seq d-quote nextloc) d-block]
   [(:seq b-quote nextloc) b-block]
   [s-quote s-str]
   [d-quote d-str]
   [b-quote b-str]
   [identifier (token 'ID (string->symbol lexeme))]
   [operator (token 'OP (string->symbol lexeme))]
   [param (token 'PARAM (string->symbol lexeme))]
   [arg (token 'ARG (string->symbol lexeme))]
   ["()" (token 'BIND (string->symbol lexeme))]
   [#\( token-LPAREN]
   [#\) token-RPAREN]
   [#\[ token-LBRACK]
   [#\] token-RBRACK]
   [#\{ (token-LBRACE!)]
   [#\} (token-RBRACE!)]
   [#\. (token 'DOT (string->symbol lexeme))]
   [#\; (token 'SEMICOLON (string->symbol lexeme))]
   [#\, (token 'COMMA (string->symbol lexeme))]
   [(:+ #\,) (rr-error (string-append "Unexpected " lexeme))]
   [(eof) (if (> _level 0)
              (cap-level! 0) 
              (void))]))

(define-macro (strlex (CUSTOM-CHARS ...) CUSTOM-RULES ...)
  #'(lexer-srcloc CUSTOM-RULES ...
                  [(:+ (:~ newline-char CUSTOM-CHARS ...)) (token 'STRING lexeme)]
                  [any-char (token 'STRING lexeme)]))

(define-macro (strlexi (CUSTOM-CHARS ...) CUSTOM-RULES ...)
  #'(strlex (#\\ CUSTOM-CHARS ...)
            ["\\{}" (token 'STRING "\\")]
            [(:seq "\\{" (:? spacetabs)) (list (token 'INTERP 0) (token-LBRACE!))]
            CUSTOM-RULES ...))

(define-macro (strlexe (CUSTOM-CHARS ...) CUSTOM-RULES ...)
  #'(strlexi (CUSTOM-CHARS ...)
             ["\\n" (token 'STRING "\n")]
             ["\\t" (token 'STRING "\t")]
             ["\\\\" (token 'STRING "\\")]
             CUSTOM-RULES ...
             [(:seq #\\ any-char) (unknown-escape)]))

(define-macro (blockstr STRLEX CUSTOM-RULES ...)
  #'(STRLEX ()
            [(:seq "\\{" nextloc "}") (escape-newline)]
            [(:seq "\\" nextloc) (str-interp)]
            [nextloc (extract-whites!)]
            CUSTOM-RULES ...))

(define-macro s-str
  #'(token-QUOTE! (strlex ()
                          [nextloc (rewind! (token-UNQUOTE!))]
                          [(eof) (cons (token-UNQUOTE!)
                                       (cap-level! 0))])))

(define-macro d-str
  #'(token-QUOTE! (strlexe (d-quote)
                           [d-quote (token-UNQUOTE!)]
                           [(:seq #\\ d-quote) (token 'STRING #\")]
                           [newline-char (unterminated-string)]
                           [(eof) (unterminated-string)])))

(define-macro b-str
  #'(token-QUOTE! (strlexi (#\( #\) #\{ #\} #\[ #\] #\,)
                           [(:or #\) #\} #\] #\, newline-char) (rewind! (token-UNQUOTE!))]
                           [#\( (push-mode-str! str-paren)]
                           [#\{ (push-mode-str! str-brace)]
                           [#\[ (push-mode-str! str-brack)]
                           [(:seq #\\ nextloc) (if (>= _dent (measure-dent!))
                                                   (rewind! #:to "\\")
                                                   (unexpected-indent))]
                           [(:seq "\\{" nextloc) (unexpected-indent)]
                           [(eof) (cons (token-UNQUOTE!)
                                        (cap-level! 0))])))

(define-macro s-block
  #'(append (list (token-QUOTE! 'DEDENT<-UNQUOTE)
                  (indent! (strlex ()
                                   [nextloc (extract-whites!)]
                                   [(eof) (cap-level! 0)])))
            (-1LF (extract-whites!))))

(define-macro d-block
  #'(append (list (token-QUOTE! (lexer-srcloc [d-quote (token-UNQUOTE!)]
                                              [any-char (unterminated-string)]
                                              [(eof) (unterminated-string)]))
                  (indent! (blockstr strlexe [(eof) (unterminated-string)])))
            (-1LF (extract-whites!))))

(define-macro b-block
  #'(append (list (token-QUOTE! 'DEDENT<-UNQUOTE)
                  (indent! (blockstr strlexi [(eof) (cap-level! 0)])))
            (-1LF (extract-whites!))))

(define-macro (push-mode-str! LEXER)
  #'(begin (push-mode! LEXER)
           (token 'STRING lexeme)))

(define-macro (pop-mode-str!)
  #'(begin (pop-mode!)
           (token 'STRING lexeme)))

(define-macro (str-mode TERMINATOR)
  #'(strlexi (#\( #\) #\{ #\} #\[ #\])
             [#\( (push-mode-str! str-paren)]
             [#\{ (push-mode-str! str-brace)]
             [#\[ (push-mode-str! str-brack)]
             [TERMINATOR (pop-mode-str!)]
             [(:or #\) #\} #\]) (rr-error "Mismatched bracket")]
             [(eof) (rr-error "Missing closing bracket")]))

(define str-paren (str-mode #\)))
(define str-brace (str-mode #\}))
(define str-brack (str-mode #\]))

(define-macro str-interp
  #'(let ([current-dent _dent]
          [next-dent (add1 _dent)]
          [new-dent (measure-dent!)])
      (cond
        [(< new-dent next-dent) (rewind! #:to "\\")]
        [(= new-dent next-dent) (let ([token-INTERP (token 'INTERP (- current-dent _level))])
                                  (begin (push-mode! (jumpto _level))
                                         (set! _level _dent)
                                         (list token-INTERP (indent!))))]
        [(> new-dent next-dent) (indentation-error next-dent)])))

(define-macro escape-newline
  #'(let ([current-dent _dent]
          [new-dent (measure-dent!)])
      (cond
        [(< new-dent current-dent) (indentation-error)]
        [(= new-dent current-dent) (token 'STRING "")]
        [(> new-dent current-dent) (indentation-error current-dent)])))

(define-macro (concat-if COND LIST)
  #'(if COND LIST empty))

(define-macro (append-if COND ITEM)
  #'(if COND (list ITEM) empty))

(define-macro (starts-with? PREFIX STR)
  #'(and (not (equal? eof STR))
         (string-prefix? STR PREFIX)))

(define-macro unterminated-string
  #'(rr-error "Unterminated string (missing closing quote)"))

(define-macro unknown-escape
  #'(rr-error (string-append "Unknown escape sequence: " lexeme)))

(define-macro unexpected-indent
  #'(rr-error "Unexpected indentation"))

(define-macro-cases indentation-error
  [(indentation-error)     #'(indentation-error 1 _dent "Insufficient indentation")]
  [(indentation-error COL) #'(indentation-error COL (- _dent COL) "Too much indentation")]
  [(indentation-error COL LENGTH MSG) #'(rr-error MSG (position-line end-pos) COL 
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

(define-macro-cases rewind!
  [(rewind! TOKEN)        #'(rewind! (string-length lexeme) TOKEN)]
  [(rewind! #:to STR)     #'(rewind! (- (string-length lexeme) (string-length STR)) (token 'STRING STR))]
  [(rewind! LENGTH TOKEN) #'(begin (file-position input-port (- (file-position input-port) LENGTH)) TOKEN)])

(define _mode main-lexer)
(define _modestack '())

(define (push-mode! lexer)
  (push! _modestack _mode)
  (set! _mode lexer))

(define (pop-mode!)
  (set! _mode (pop! _modestack))
  (if (jumpto? _mode)
      (begin (set! _level (jumpto-level _mode))
             (pop-mode!))
      void)
  _mode)

(struct jumpto (level))
(define _level 0)
(define _dent 0)

(define-macro (cap-level! CAP)
  #'(let* ([dedents '()]
           [_ (while (> _level CAP)
                     (set! dedents (append dedents (dedent!))))]
           [num-feed (if (empty? dedents) numLF (sub1 numLF))])
      (append (concat-if (> num-feed 0)
                         (make-list num-feed (token-NEWLINE)))
              dedents
              (append-if (equal? _mode main-lexer) (token-NEWLINE))
              (append-if (and (> _level 0)
                              (> _dent _level))
                         (token-STRING (- _dent _level) #\tab)))))

(define-macro (measure-dent!)
  #'(begin (set! _dent (string-length (last (string-split lexeme "\n" #:trim? #f))))
           _dent))

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

(define-macro extract-whites!
  #'(cap-level! (min _level (measure-dent!))))

(define-macro -1LF
  #'(let ([whites extract-whites!])
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
  (token 'LPAREN "("))

(define token-RPAREN
  (token 'RPAREN ")"))

(define token-LBRACK
  (token 'LBRACK "["))

(define token-RBRACK
  (token 'RBRACK "]"))

(define (token-LBRACE! [lexer main-lexer])
  (push-mode! lexer)
  (token 'LBRACE "{"))

(define-macro token-RBRACE!
  #'(cond [(empty? _modestack) (rr-error "No matching pair")]
          [else (pop-mode!) (token 'RBRACE "}")]))

(define (token-QUOTE! lexer)
  (push-mode! lexer)
  (token 'QUOTE _mode))

(define (token-UNQUOTE!)
  (pop-mode!)
  (token 'UNQUOTE _mode))

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
