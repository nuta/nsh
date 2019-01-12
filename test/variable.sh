a=123
b=456

echo $a
echo $b
echo ${undefined}
echo ${undefined-}
echo ${undefined-hello}
echo ${undefined:-0${a}45}
