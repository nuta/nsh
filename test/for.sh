for x in foo bar a b c d e
do
    if [ $x = a ]
    then
        break
    fi

    echo $x
done
