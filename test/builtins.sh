source
source test/builtins.sh.source
echo $msg

func1() {
    echo $1
    shift
    echo $1
    shift
    echo $1
    shift
    echo $1
    shift
    echo $1
    shift
}

func1 a b "c d"

echo "unset --------------"
foo=123
echo $foo
unset foo
echo $foo

exit 0
echo unreachable
