#lang br/quicklang
(provide (rename-out [b-module-begin #%module-begin]))

(define-macro (b-module-begin (expres EXPR))
  #'(#%module-begin EXPR))

(define-macro (applyJ A COMMA ...)
  #'(foldl (lambda (f a) (f a))
           A `(,COMMA ...)))

(define-macro (j X ...)
  #'(lambda (a) (foldl (lambda (f a) (f a)) a `(,(comma X) ...) )))

(define-macro-cases comma
  [(comma (op OP))    #'(lambda (x) `(OP #:post ,x))]
  [(comma (dot PROP)) #'(lambda (x) `(prop PROP ,x))]
  [(comma E)          #'(lambda (x) `(,x E))])

(define-macro (applyO E OP)
  #'`(,OP #:post ,E))

(define (op? x)
  (case x
    [(+ / - > < ?) #t]
    [else #f]))

(define-macro-cases apply0
  [(apply0 E (dot . PROP)) #'`(prop PROP ,E)]
  [(apply0 E E2)           #'`(,E ,E2)]
  [(apply0 E (id X ... Z)) #'`(disambiguate ,E ,(if (op? 'Z) 'A 'B))]
  )

(define-macro (dot . PROP)
  #''(prop PROP this))

(define-macro (atom A ...)
  #''(atom A ...))

(define (nuke->keyword a)
  (let ([s (symbol->string a)])
    (string->keyword (substring s 0 (- (string-length s) 1)))))

(define-macro (kv0 K ... V)
  #'(append `(,(nuke->keyword 'K) ,V) ...))

(define-macro-cases id
  [(id X)   #''X]
  [(id . X) #''(disambiguate . X)])

(define-macro (op OP) #''OP)

(define-macro-cases apply1
  [(apply1 E E2)           #'(append `(E) `(E2))])

(define-macro (braces X)
  #'`(quote ,X))

(define num    identity)
(define string identity)
(define paren  identity)
(define applyG list)

(define-macro (nuke->kw NUKE)
  #''NUKE)

(provide atom num string paren braces dot op id)
(provide applyJ j apply1 applyG apply0 applyO kv0)
