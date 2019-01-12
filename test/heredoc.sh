foo=1234
cat << EOF > /dev/stderr
hello $foo
world
EOF
echo after heredoc > /dev/stderr
