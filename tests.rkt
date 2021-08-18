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
         (_Qx
          let
          (_Qx scores (_Qx = (group (applyC1 (applyC1 (applyO brace 1) 2) 3)))))
         (apply3
          (_Qx
           let
           (_Qx
            avg-score
            (_Qx
             =
             (applyE1
              (applyEO
               (_Q_2
                (apply0 (apply0 scores (prop reduce)) (group (applyO paren)))
                (_Q_2
                 (applyO (keyword to :) 0)
                 (_Q_2
                  ->
                  (apply2
                   (apply1 (alias (label : a) (label : sum)) (label : score))
                   (apply1 (applyO sum +) score)))))
               /)
              (apply0 (apply0 scores (prop count)) (group (applyO paren)))))))
          (apply1 println avg-score)))))

(test "let top10avg = {,: sort,: reverse,: first 10,: average}"
      '(return
        (_Qx
         let
         (_Qx
          top10avg
          (_Qx
           =
           (group
            (applyC1
             (applyC1
              (applyC1 (applyC1 (applyO brace) piped-to: sort) piped-to: reverse)
              piped-to:
              (apply1 first 10))
             piped-to:
             average)))))))

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
         (_Qx
          let
          (_Qx
           (apply0
            prompt
            (group
             (applyC1
              (apply1 (applyO paren (label : text)) String)
              (alias (label : then) (label : callback)))))
           (_Qx => (applyC1 (applyC1 (apply1 print text) readln) callback))))
         (apply3
          (_Q_2
           let
           (_Q_2
            (apply0
             unless
             (group
              (apply1
               (applyO paren (label : unwanted))
               (apply1 (label : x) (alias (label : then) (label : replacement))))))
            (_Q_2
             =
             (apply3
              (_Qx (applyCO (apply1 (applyO x ==) unwanted) ?) (_Qx -> replacement))
              (_Qx (keyword else :) (_Qx -> x))))))
          (apply3
           (_Qx prompt (_Qx name (_Qx = (string "Your name: "))))
           (apply1
            println
            (string
             "Hello "
             (group
              (applyC1
               (applyC1 (applyO brace name) piped-to: (apply1 unless (string)))
               (applyO (keyword then :) (string "World")))))))))))

(test #<<EOF
let unless(:unwanted :x :then:replacement) =
	is x, unwanted,? -> replacement
	else: -> x

let is(:x :y) =
	y,:
		: Predicate, =>:(apply to:x)
		:            => x== y

let
	apply(:f :to:x) => f x
	Predicate = @{: => Boolean}
EOF
      '(return
        (apply3
         (_Q_2
          let
          (_Q_2
           (apply0
            unless
            (group
             (apply1
              (applyO paren (label : unwanted))
              (apply1 (label : x) (alias (label : then) (label : replacement))))))
           (_Q_2
            =
            (apply3
             (_Qx (applyCO (applyC1 (apply1 is x) unwanted) ?) (_Qx -> replacement))
             (_Qx (keyword else :) (_Qx -> x))))))
         (apply3
          (_Q_2
           let
           (_Q_2
            (apply0 is (group (apply1 (applyO paren (label : x)) (label : y))))
            (_Q_2
             =
             (apply2
              y
              piped-to:
              (apply3
               (applyC1
                (apply1 (label :) Predicate)
                (applyO
                 (keyword => :)
                 (group (apply1 (applyO paren apply) (applyO (keyword to :) x)))))
               (_Qx (label :) (_Qx => (apply1 (applyO x ==) y))))))))
          (apply2
           let
           (apply3
            (_Qx
             (apply0
              apply
              (group
               (apply1 (applyO paren (label : f)) (alias (label : to) (label : x)))))
             (_Qx => (apply1 f x)))
            (_Qx
             Predicate
             (_Qx
              =
              (apply0
               @
               (group (_Qx (applyO brace (label :)) (_Qx => Boolean))))))))))))