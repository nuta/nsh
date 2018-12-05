echo -e "abc\ndef ghi" | while read -p this_is_prompt var; do
    echo $var
done
