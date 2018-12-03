echo abc | while read -p this_is_prompt var; do
    echo $var
done
