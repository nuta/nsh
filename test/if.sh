# A blackbox tests.

func1() {
    if [ $foo -eq 123456 ]; then
        echo "error: #1"
        exit 1
    fi

    if [ $foo -ne 123 ]; then
        echo "error: #2"
        exit 1
    elif [ $foo -gt 456 ]; then
        echo "error: #3"
        exit 1
    elif false; then
        echo "error: #4"
        exit 1
    else
        echo "else else else"
    fi

    foo="this is a string"
}

foo=${bar:=123}
func1

if [ "$foo" != "this is a string" ]; then
    echo "error: #5"
    exit 1
fi

echo "ok!"
