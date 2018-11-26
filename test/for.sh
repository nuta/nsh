# Break from a nested for & if.
for x in loop1----- loop2----- loop3------
do
    echo $x
    for y in foo bar a b c d e
    do
        if [ $y = a ]
        then
            if [ 1 -eq 1 ]
            then
                break
            fi
        fi

        echo $y
    done
done
