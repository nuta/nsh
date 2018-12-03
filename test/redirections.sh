cd $(mktemp -d)
echo hello > hello.txt
cat hello.txt
ls > /dev/null | echo message > message.txt | hexdump -C
echo message > message.txt
cat message.txt
