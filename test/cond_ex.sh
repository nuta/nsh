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
    echo "BAD #1"
fi

if [[ $a -gt 122 ]]; then
    echo "OK #4"
fi

if [[ "this is it" == "this is it" ]]; then
    echo "OK #5"
else
    echo "BAD #2"
fi

if [[ "this is it" != "this is i" ]]; then
    echo "OK #6"
else
    echo "BAD #3"
fi

if [[ 123 -eq $a && "$a" == "123" ]]; then
    echo "OK #7"
else
    echo "BAD #4"
fi

if [[ 456 -eq $a || "$a" == "123" ]]; then
    echo "OK #8"
else
    echo "BAD #5"
fi
