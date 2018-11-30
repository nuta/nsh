a=$(( 1 ))
b=$(( 1 + 1 ))
c=$(( b + 1 ))
echo $a $b $c

x=$(echo $a)
y=$(echo $b)
echo $x
echo $y
echo $((x+y))

func1() {
    echo $(($1-1))
}
z=$(func1 7)
echo $((z*z))
