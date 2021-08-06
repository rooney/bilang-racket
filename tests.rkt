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
          (applyL scores (applyL = (group (applyK (applyK (applyO brace 1) 2) 3)))))
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

(test #<<EOF
let prompt(:text String, :then:callback) => print text, readln, callback

let unless(:unwanted :x :then:replacement) =
 x== unwanted,? -> replacement
 else: -> x

prompt name = 'Your name: '
println "Hello #{name,: unless '', then:'World'}"
EOF
      '(return
        (apply3
         (applyK
          (applyK
           (applyL
            let
            (applyL (apply0 prompt (group (applyK (apply1 (applyO paren (label : text)) String) (alias (label : then) (label : callback))))) (applyL => (apply1 print text))))
           readln)
          callback)
         (apply3
          (applyZ
           let
           (applyZ
            (apply0 unless (group (apply1 (applyO paren (label : unwanted)) (apply1 (label : x) (alias (label : then) (label : replacement))))))
            (applyZ = (apply3 (applyL (commaOP (apply1 (applyO x ==) unwanted) ?) (applyL -> replacement)) (applyL (keyword else :) (applyL -> x))))))
          (apply3
           (applyL prompt (applyL name (applyL = (string "Your name: "))))
           (apply1 println (string "Hello " (group (applyK (pipe-1 (applyO brace name) (apply1 unless (string))) (applyO (keyword then :) (string "World")))))))))))
      