ls | echo hello | hexdump -C
for x in 1 2 "3 4"; do echo $x; done
{ echo hello; echo world; foo=123 env | grep foo; } | hexdump -C

msg1=guten
( echo $msg1; msg1=tag; echo $msg1 ) | hexdump -C
echo $msg1
