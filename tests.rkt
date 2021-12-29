#lang racket
(require "parser.rkt" "tokenizer.rkt" brag/support rackunit)

(define (test source result)
	(check-equal?
	 (parse-to-datum
		(apply-tokenizer-maker make-tokenizer source))
	 result))

(test #<<EOF
let scores = [9, 10, 9]
let avg-score = \
	scores.reduce() to:0 -> :a:sum :score
		sum+ score
	/ scores.count()
output avg-score
EOF
			'(return
				(apply3
				 (_Qx
					let
					(_Qx scores (_Qx = (grouping (applyC1 (applyC1 (applyO brace 1) 2) 3)))))
				 (apply3
					(_Qx
					 let
					 (_Qx
						(apply0 avg - score)
						(_Qx
						 =
						 (applyE1
							(applyEO
							 (_Q_2
								(apply0 (apply0 scores (dot reduce)) (grouping (applyO paren)))
								(_Q_2
								 (applyO (keyword to :) 0)
								 (_Q_2
									->
									(apply2
									 (apply1 (alias (label : a) (label : sum)) (label : score))
									 (apply1 (apply0 sum +) score)))))
							 /)
							(apply0 (apply0 scores (dot count)) (grouping (applyO paren)))))))
					(apply1 output (apply0 avg - score))))))

(test "let top10avg = {,: sort,: reverse,: first 10,: average}"
			'(return
				(_Qx
				 let
				 (_Qx
					top10avg
					(_Qx
					 =
					 (grouping
						(applyC1
						 (applyC1
							(applyC1 (applyC1 (applyO brace) piped-to: sort) piped-to: reverse)
							piped-to:
							(apply1 first 10))
						 piped-to:
						 average)))))))


(test #<<EOF
.grade(_) =
	.attendance> minimum-attendance, else: -> 'F
	.test-results.count()> 0, else: -> 'N/A
	.test-results.map() {.score},: max,:
		: {>= 90} => 'A
		: {>= 75} => 'B
		: {>= 60} => 'C
		:         => 'D
EOF
			'(return
				(_Q_2
				 (apply0 (dot grade) (grouping (applyO paren _)))
				 (_Q_2
					=
					(apply3
					 (applyCQ (apply1 (apply0 (dot attendance) >) (apply0 minimum - attendance)) (_Qx (keyword else :) (_Qx -> (string "F"))))
					 (apply3
						(applyCQ (apply1 (applyO (apply0 (apply0 (apply0 (dot test) - results) (dot count)) (grouping (applyO paren))) >) 0) (_Qx (keyword else :) (_Qx -> (string "N/A"))))
						(apply2
						 (applyC1 (apply1 (apply0 (apply0 (dot test-results) (dot map)) (grouping (applyO paren))) (grouping (applyO brace (dot score)))) piped-to: max)
						 piped-to:
						 (apply3
							(_Qx (label :) (_Qx (grouping (apply1 (applyO brace >=) 90)) (_Qx => (string "A"))))
							(apply3
							 (_Qx (label :) (_Qx (grouping (apply1 (applyO brace >=) 75)) (_Qx => (string "B"))))
							 (apply3 (_Qx (label :) (_Qx (grouping (apply1 (applyO brace >=) 60)) (_Qx => (string "C")))) (_Qx (label :) (_Qx => (string "D")))))))))))))

(test "let top10avg = {,: sort,: reverse,: first 10,: average}"
			'(return
				(_Qx
				 let
				 (_Qx
					top10avg
					(_Qx
					 =
					 (grouping
						(applyC1
						 (applyC1
							(applyC1 (applyC1 (applyO brace) piped-to: sort) piped-to: reverse)
							piped-to:
							(apply1 first 10))
						 piped-to:
						 average)))))))



(test #<<EOF
let prompt(:text String, :then:callback) =
output text
	input then:callback

let unless(:unwanted :x :else:replacement) =
	x== unwanted,? -> replacement
	else: -> x

prompt "Your name: ", -> :name
output "
	Hello #!{name,: unless "", else:"World"}
"
EOF
			'(return
				(apply3
				 (_Qx
					let
					(_Qx
					 (apply0
						prompt
						(grouping
						 (applyC1
							(apply1 (applyO paren (label : text)) String)
							(alias (label : then) (label : callback)))))
					 (_Qx => (applyC1 (applyC1 (apply1 output text) (apply0 input (dot line))) callback))))
				 (apply3
					(_Q_2
					 let
					 (_Q_2
						(apply0
						 unless
						 (grouping
							(apply1
							 (applyO paren (label : unwanted))
							 (apply1 (label : x) (alias (label : else) (label : replacement))))))
						(_Q_2
						 =
						 (apply3
							(_Qx (applyCO (apply1 (applyO x ==) unwanted) ?) (_Qx -> replacement))
							(_Qx (keyword else :) (_Qx -> x))))))
					(apply3
					 (applyCQ (apply1 prompt (string "Your name: ")) (_Qx -> (label : name)))
					 (apply1 output (string "Hello " (grouping (applyC1 (applyC1 (applyO brace name) piped-to: (apply1 unless (string))) (applyO (keyword else :) (string "World")))))))))))

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
						(grouping
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
						(apply0 is (grouping (apply1 (applyO paren (label : x)) (label : y))))
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
								 (grouping (apply1 (applyO paren apply) (applyO (keyword to :) x)))))
							 (_Qx (label :) (_Qx => (apply1 (applyO x ==) y))))))))
					(apply2
					 let
					 (apply3
						(_Qx
						 (apply0
							apply
							(grouping
							 (apply1 (applyO paren (label : f)) (alias (label : to) (label : x)))))
						 (_Qx => (apply1 f x)))
						(_Qx
						 Predicate
						 (_Qx
							=
							(apply0
							 @
							 (grouping (_Qx (applyO brace (label :)) (_Qx => Boolean))))))))))))

(test #<<EOF
output '
	<table class='theme-`
		db.query1 '
			SELECT name FROM themes
			WHERE user-id = `
				session.user.id
		else: 'default
	'>
		<tr>
			<td>`
				db.query 'SELECT * FROM entries
				,.join() '
							</td>
						</tr>
						<tr>
							<td>
			</td>
		</tr>
	</table>
EOF
			'(return
				(apply1
				 output
				 (string
					"<table class='theme-"
					(grouping
					 (apply3
						(apply1 (apply0 db (dot query1)) (string "SELECT name FROM themes" "\n" "WHERE user-id = " (grouping (apply0 (apply0 session (dot user)) (dot id)))))
						(_Qx (keyword else :) (string "default"))))
					"'>"
					"\n"
					"\t"
					"<tr>"
					"\n"
					"\t\t"
					"<td>"
					(grouping
					 (applyE1
						(applyEO (apply1 (apply0 db (dot query)) (string "SELECT * FROM entries")) (apply0 (dot join) (grouping (applyO paren))))
						(string "\t\t" "</td>" "\n" "\t" "</tr>" "\n" "\t" "<tr>" "\n" "\t\t" "<td>")))
					"\n"
					"\t\t"
					"\n"
					"</td>"
					"\n"
					"\t"
					"</tr>"
					"\n"
					"</table>"))))