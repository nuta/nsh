# https://en.wikipedia.org/wiki/Process_substitution
cat <(echo hello; echo world > /dev/stderr) | hexdump -C
