Blackbox tests
==============

## Running
To run all tests:
```
$ ./tools/run-blackbox-tests.py
```

To run some tests:
```
$ ./tools/run-blackbox-tests.py test/if.sh test/for.sh
```

## Adding a new test
Just add these files:
- `<name>.sh`: A shell script run by nsh. It must exit with 0 on success.
- `<name>.stdout`: The expected stdout from the script.
- `<name>.stderr`: The expected stderr from the script (optional).
