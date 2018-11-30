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
    exit 0
}

func1 a b "c d"
