Blackbox tests
==============

## Running
To run all tests:
```
$ ./run-tests.py
```

To run some tests:
```
$ ./run-tests.py test/if.sh test/for.sh
```

## Adding a new test
Just add these files:
- `<name>.sh`: A shell script run by nsh.
- `<name>.stdout`: The expected stdout from the script.
- `<name>.stderr`: The expected stderr from the script (optional).
