#lang racket
(require "parser.rkt" "tokenizer.rkt" brag/support rackunit)

(define (test source result)
  (check-equal?
   (parse-to-datum
    (apply-tokenizer-maker make-tokenizer source))
   result))

(test #<<EOF
let scores = {1, 2, 3}
let avg-score = \
 scores.reduce() to:0 -> :a:sum :score
  sum+ score
 ,/ scores.count()
println avg-score
EOF
      '(return
        (apply3
         (applyL
          let
          (applyL
           scores
           (applyL = (group (applyK (applyK (apply0 brace 1) 2) 3)))))
         (apply3
          (applyL
           let
           (applyL
            avg-score
            (applyL
             =
             (applyK
              (commaOP
               (applyZ
                (apply0 (apply0 scores (dot reduce)) (group paren))
                (applyZ
                 (applyO (keyword to :) 0)
                 (applyZ
                  ->
                  (apply2
                   (apply1 (alias (label : a) (label : sum)) (label : score))
                   (apply1 (applyO sum +) score)))))
               /)
              (apply0 (apply0 scores (dot count)) (group paren))))))
          (apply1 println avg-score)))))

(test "let top10avg = {,: sort,: reverse,: first 10,: average}"
      '(return (applyL let (applyL top10avg (applyL = (group (pipe-1 (pipe-1 (pipe-1 (pipe-1 brace sort) reverse) (apply1 first 10)) average)))))))