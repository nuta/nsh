# Break from a nested for & if.
for x in loop1----- loop2----- loop3------
do
    echo $x
    for y in a b b b c d e f g
    do
        if [ $y = e ]
        then
            if [ 1 -eq 1 ]
            then
                break
            fi
        fi

        if [ $y = b ]
        then
            continue
        fi

        echo $y
    done
done
