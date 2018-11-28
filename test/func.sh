var1=123
func1() {
    var1=456
}
func1
echo $var1

# local
var2=123
function func2 () {
    local var1=456
}
func2
echo $var2

# FIXME: fix parse error
# recursion
fib() {
    if [ $1 -lt 2 ]; then
        echo $1
        return
    fi
    x=$(( $1 - 1 ))
    y=$(( $1 - 2 ))
    a=$(fib $x)
    b=$(fib $y)
    echo $(( a + b ))
}

fib 7
