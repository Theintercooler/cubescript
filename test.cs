a = "1"
a = (+ $a "2")
// A comment
/*
    A long comment
    That is cool
*/

if (< $a 10) "echo [lol]" [ ]

if (true) [
    echo "Hello world"
    echo (concatword "Hello" "-" "World" "!")
] [
    echo "You won't see this"
]

if (< 0 -1) [
    
] [
    echo "Hello world!"
    echo (concat "Hello" "World" "Spaces!!")
]

exec "test.cs"

