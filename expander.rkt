#lang br/quicklang
(provide (rename-out [b-module-begin #%module-begin]))

(define-macro (b-module-begin (program LINE ...))
  (with-pattern ()
    #'(#%module-begin
       (module configure-runtime racket
       'LINE ...
       ))))
