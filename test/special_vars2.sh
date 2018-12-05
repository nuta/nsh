
echo "\$? ---------"
echo $?
echo bar | grep foo
echo $?
echo bar | grep bar
echo $?
echo "\$0 ---------"
echo $0
echo "\$1 ---------"
echo $1
echo "\$2 ---------"
echo $2

func() {
    echo "\$# ---------"
    echo $#
    echo "\$* ---------"
    for x in $*; do
        echo $x
    done
    echo "\$@ ---------"
    for x in $@; do
        echo $x
    done
    echo "\"\$*\" ---------"
    for x in "$*"; do
        echo $x
    done
    echo "\"\$@\" ---------"
    for x in "$@"; do
        echo $x
    done

    shift

    echo "\"\$@*\" ---------"
    for x in "$*"; do
        echo $x
    done
    echo "\"\$@\" ---------"
    for x in "$@"; do
        echo $x
    done

    echo "\$0 ---------"
    echo $0
    echo "\$1 ---------"
    echo $1
    echo "\$2 ---------"
    echo $2
}

func "spam egg" breakfast
