#!/usr/bin/env python3
import argparse
import os
import sys
import subprocess
from pathlib import Path
from termcolor import cprint

def run_test(test):
    cprint(f"Running {test}...", attrs=["bold"], end="")
    sys.stdout.flush()

    expected_stdout_path = Path(test).with_suffix(".stdout")
    expected = open(expected_stdout_path).read()

    # Before running nsh, make sure that bash outputs expected stdout.
    bash_stdout = subprocess.check_output(["bash", test]).decode("utf-8")
    if bash_stdout.rstrip() != expected.rstrip():
        cprint(f"unexpected bash output (fix {expected_stdout_path}!)", "red", attrs=["bold"])
        print("expected ----------------------------")
        print(expected)
        print("bash stdout -------------------------")
        print(stdout)
        return

    try:
        stdout = subprocess.check_output(
            ["./target/debug/nsh", test],
            env={
                "RUST_BACKTRACE": "1",
                "RUST_LOG": "nsh=trace",
            }.update(os.environ)
        ).decode("utf-8")
    except subprocess.CalledProcessError as e:
        cprint(f"exited with {e.returncode}", "red", attrs=["bold"])
        return

    if stdout.rstrip() == expected.rstrip():
        cprint("ok", "green", attrs=["bold"])
    else:
        cprint("unexpected stdout", "red", attrs=["bold"])
        print("expected ----------------------------")
        print(expected)
        print("stdout ------------------------------")
        print(stdout)

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("tests", nargs="*")
    args = parser.parse_args()

    if len(args.tests) > 0:
        tests = args.tests
    else:
        tests = Path("test").glob("*.sh")

    subprocess.run(["cargo", "build"])

    for test in tests:
        run_test(test)

if __name__ == "__main__":
    main()
