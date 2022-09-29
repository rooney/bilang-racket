```coffeescript
prompt name = "Please enter your name: "
println "
	Hello `{name |> if =:"" then: "World"}
"
```