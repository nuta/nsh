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
bar=123
eval foo=$bar
echo $foo
unset foo
echo $foo

echo exit
exit 0
echo unreachable
