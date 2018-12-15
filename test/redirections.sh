# cd $(mktemp -d)
cd /tmp
rm -f message.txt
echo message1 > message.txt
echo message2 >> message.txt
ls > /dev/null | echo hello > hello.txt | hexdump -C
sh -c "this_command_does_not_exist" > stderr.txt 2>&1
cat hello.txt
cat message.txt
cat stderr.txt
