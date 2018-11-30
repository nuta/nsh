# disable-output-check
TO_BE_EXPORTED="this is exported value"
export
export TO_BE_EXPORTED
export

set -e
env | grep TO_BE_EXPORTED
THIS_IS_ASSIGNMENT=123 env | grep THIS_IS_ASSIGNMENT
