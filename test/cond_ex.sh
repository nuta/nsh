a=123

if [[ $a -eq 123 ]]; then
    echo "OK #1"
fi

if [[ $a -ne 567 ]]; then
    echo "OK #2"
fi

if [[ $a -le 123 ]]; then
    echo "OK #3"
fi

if [[ $a -lt 123 ]]; then
    echo "BAD #4"
else
    echo "OK #4"
fi

if [[ $a -gt 122 ]]; then
    echo "OK #5"
fi

if [[ "this is it" == "this is it" ]]; then
    echo "OK #6"
else
    echo "BAD #6"
fi

if [[ "this is it" != "this is i" ]]; then
    echo "OK #7"
else
    echo "BAD #7"
fi

if [[ 123 -eq $a && "$a" == "123" ]]; then
    echo "OK #8"
else
    echo "BAD #8"
fi

if [[ 456 -eq $a || "$a" == "123" ]]; then
    echo "OK #9"
else
    echo "BAD #9"
fi

if [[ "$a" == ?23 ]]; then
    echo "OK #10"
else
    echo "BAD #10"
fi

if [[ "$a" == *3 ]]; then
    echo "OK #11"
else
    echo "BAD #11"
fi

if [[ "$a" == *2 ]]; then
    echo "BAD #12"
else
    echo "OK #12"
fi
