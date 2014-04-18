i = 5

a = [
    i = 1
    b = [
        echo $arg@[ + $i 1 ] @@i $i
    ]

    i = 2

    b "test" "This"
]

a "Hello world"

i = 2

a "Again?"