#lang br/quicklang
(require "parser.rkt" "tokenizer.rkt")

(module+ reader
  (provide read-syntax))

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port path)))
  (println parse-tree)
  (strip-bindings
   #`(module bilang-mod bilang/expander
       #,parse-tree)))
