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
.grade(:) =
	.attendance> minimum-attendance, else: -> 'F'
	.test-results.count()> 0, else: -> 'N/A'
	.test-results.map() {.score},: max,:
		: {>= 90} => 'A'
		: {>= 75} => 'B'
		: {>= 60} => 'C'
		:         => 'D'
EOF
      '(return
        (_Q_2
         (apply0 (prop grade) (group (applyO paren (label :))))
         (_Q_2
          =
          (apply3
           (applyCQ (apply1 (prop attendance >) minimum-attendance) (_Qx (keyword else :) (_Qx -> (string "F"))))
           (apply3
            (applyCQ (apply1 (applyO (apply0 (apply0 (prop test-results) (prop count)) (group (applyO paren))) >) 0) (_Qx (keyword else :) (_Qx -> (string "N/A"))))
            (apply2
             (applyC1 (apply1 (apply0 (apply0 (prop test-results) (prop map)) (group (applyO paren))) (group (applyO brace (prop score)))) piped-to: max)
             piped-to:
             (apply3
              (_Qx (label :) (_Qx (group (apply1 (applyO brace >=) 90)) (_Qx => (string "A"))))
              (apply3
               (_Qx (label :) (_Qx (group (apply1 (applyO brace >=) 75)) (_Qx => (string "B"))))
               (apply3 (_Qx (label :) (_Qx (group (apply1 (applyO brace >=) 60)) (_Qx => (string "C")))) (_Qx (label :) (_Qx => (string "D")))))))))))))

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

let unless(:unwanted :x :else:replacement) =
	x== unwanted,? -> replacement
	else: -> x

prompt name = 'Your name: '
println "Hello #!{name,: unless '', else:'World'}"
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
               (apply1 (label : x) (alias (label : else) (label : replacement))))))
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
               (applyO (keyword else :) (string "World")))))))))))

(test #<<EOF
let unless(:unwanted :x :else:replacement) =
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
              (apply1 (label : x) (alias (label : else) (label : replacement))))))
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

(test #<<EOF
progress.complete? '
	<p class='#!{
		sql #!'
			SELECT 'name' FROM 'themes'
			WHERE 'user_id' = '#!{user.id}'
		, -> :theme
			'theme-#!{theme.name}'
		else:
			'default'
	}'>
		Word,

		Sentence One.
		Sentence Two.
	</p>
'
else: #!"
	<div class="loading" data-progress=
		"#!{progress%}%"/>
EOF
      '(return
        (apply3
         (apply1
          (apply0 progress (prop complete ?))
          (string
           "<p class="
           "'"
           (group
            (apply2
             (applyO brace)
             (apply3
              (applyEZ
               (apply1 sql (string "SELECT 'name' FROM 'themes'" "\n" "WHERE 'user_id' = '" (group (applyO brace (apply0 user (prop id)))) "'" "\n"))
               (_Q_2 -> (apply2 (label : theme) (string "theme-" (group (applyO brace (apply0 theme (prop name))))))))
              (_Q_2 (keyword else :) (string "default")))))
           "'"
           ">"
           "\n"
           "\t"
           "Word,"
           "\n"
           "\n"
           "\t"
           "Sentence One."
           "\n"
           "\t"
           "Sentence Two."
           "\n"
           "</p>"))
         (_Qx (keyword else :) (string "<div class=\"loading\" data-progress=" "\n" "\t" "\"" (group (applyO brace (applyO progress %))) "%\"/>")))))


