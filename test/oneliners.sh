ls | echo hello | hexdump -C
for x in 1 2 "3 4"; do echo $x; done
{ echo hello; echo world; foo=123 env | grep foo; } | hexdump -C
