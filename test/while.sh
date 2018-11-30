while_func() {
    while [ "$1" != "" ]; do
        echo $1
        shift
        echo next is $1
    done
}

while_func a b c d
