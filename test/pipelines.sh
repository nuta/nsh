echo foo; echo bar
echo hello | echo world | hexdump -C
THIS_IS_ENV_VAR=abc echo | THIS_IS_ENV_VAR=123 env | grep THIS_IS_ENV_VAR
