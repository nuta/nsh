# disable-output-check
TO_BE_EXPORTED="this is exported value"
export
export TO_BE_EXPORTED
export
env | grep TO_BE_EXPORTED
