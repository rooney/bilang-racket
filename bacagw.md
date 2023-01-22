```
prompt name = "What is your name? "
println "
	Hello `{name |> if =:"" "World"}
"
```

```
prompt name = "Name: "
println "
	Hello @:{name |> if =:"" "World"}
"
```

n.* factorial n.- 1
fib n.- 2,.+ fib n.- 1
fib n- 2,+ fib n- 1

@material.Button
@(material.Button)


user-name
user-password
-foo-bar
-foo->bar
-foo.bar

-p.x+ 1
-p.x,+ 1
-(p.x)+ 1
-(p.x),+ 1

-p.x
-p-x

-3.1
-(3.1)
+(3.5)
(1.1)-(2.2)
f-(2.2)
"f"-(2.2)
[f]-(2.2)

5a-b 5(a-b)
5a.b
(f)a.b
[x]a.b
"s"a.b
(f)a-b

"f"-x

(f)a+
(f)a.x+
"f"a.x+
f x,(upper)"abc"5.abs+"a"

f x,-(square x)
f x,(square x)~
f x,(map double)(filter odd, numbers)

f+(1)


2(x+ 1)
(2)x+1
2file-size
mode/read-only


+(+) a


:a Mammal, Aquatic
:a @:[Integer String]
:a 1-of:[0 1]

.a: 1
.b: 2
]


:multiplier [Integer], `NaN, (`NaN)
:multiplier [Integer], (1)
:multiplier [Integer], 1, 1
:multiplier [Integer], default:1
:multiplier [Integer],= 1
:multiplier [Integer], =:1


:this [Integer], (f x), {this}
:{[Integer]} {f x}

:this [Integer], {this}, f x

:nama: [String]
.nama "Pandu"
.nama : [String] ("Pandu")




::n [Integer], {[Integer] => n* n}

: 0 -> 1
::n   [Integer {,> 0}]
    |
	|
	n* factorial n- 1


: 0, 1, -> 1
::n   [Integer {,> 1}]
	| [Integer {,> 1}]
	|
	fib n- 1,+ fib n- 2


::multiplier   [Integer], (0)
::demultiplier [Integer]
	|		   [Integer]
	|
	fib n- 2,+ fib n- 1

:x    [Integer], [String], (0)
:b    [Integer]
	| [Integer]
	|
	a+ b


:x	  []
:f    {: x -> :y}
	| y
	|
	f x


:=:reject []
:sub      []
:x        []
	| x, sub
	|
	x == reject ?
		sub
	x



:x	  []
:f    [{: x -> :y}]
	| y
	|
	f x



:numbers [[[]]]

:(thunk):

[a :[rest]:]
[a rest..]

:countdown [[[ 5 4 3 2 1 ]]]
:items [:h ..:tail(: ::) ]
:items [:h :: ..:]
:items [:tag: :: ..:]



:person [:first-name:fn :last-name :address:addr[:street :city :country]]
:numbers [:h ::mid :t]


"
	There are `{n} `
		n> 1
			pluralize item.name
		else:
			item.name
"

"
	There are \{n} \
		n> 1
			pluralize item.name
		else:
			item.name
	
"

"hello ..{}{aa}"
"hello ::{name}"
"hello `{name}"


[
	.: Animal
		.latin-name: "Canis familiaris"

	:feed:   [String]
	.hobby : [String] ("sleeping")
	.dangerous: true
]
