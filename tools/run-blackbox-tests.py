#!/usr/bin/env python3
import argparse
import os
import sys
import subprocess
from pathlib import Path
from termcolor import cprint
import re

def run_test(test):
    cprint(f"Running {test}...", attrs=["bold"], end="")
    sys.stdout.flush()

    expected_stdout_path = Path(test).with_suffix(".stdout")
    expected_stderr_path = Path(test).with_suffix(".stderr")

    try:
        expected_stdout = open(expected_stdout_path).read()
    except FileNotFoundError:
        expected_stdout = ""

    try:
        expected_stderr = open(expected_stderr_path).read()
    except FileNotFoundError:
        expected_stderr = ""

    test_body = open(test).read()
    disable_output_check = "disable-output-check" in test_body

    m = re.search(r"exit-with=([0-9]+)", test_body)
    if m is None:
        expected_returncode = 0
    else:
        expected_returncode = int(m.groups()[0])

    # Before running nsh, make sure that bash outputs expected stdout.
    p = subprocess.Popen(["bash", test], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    bash_returncode = p.wait()
    bash_stdout = p.stdout.read().decode("utf-8")
    bash_stderr = p.stderr.read().decode("utf-8")

    if bash_returncode != expected_returncode:
        cprint(f"bash returned {bash_returncode} (expected {expected_returncode})", "red", attrs=["bold"])
        print("bash stdout -------------------------")
        print(bash_stdout)
        print("bash stderr -------------------------")
        print(bash_stderr)
        return

    if disable_output_check == False and bash_stdout.rstrip() != expected_stdout.rstrip():
        cprint(f"unexpected bash output (fix {expected_stdout_path}!)", "red", attrs=["bold"])
        print("expected ----------------------------")
        print(expected_stdout)
        print("bash stdout -------------------------")
        print(bash_stdout)
        print("bash stderr -------------------------")
        print(bash_stderr)
        return

    # Run the test.
    p = subprocess.Popen(
        ["./target/debug/nsh", test],
        env={
            "RUST_BACKTRACE": "1",
            "RUST_LOG": "nsh=trace",
        }.update(os.environ),
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE
    )

    returncode = p.wait()
    stdout = p.stdout.read().decode("utf-8")
    stderr = p.stderr.read().decode("utf-8")

    if disable_output_check == False and stdout.rstrip() != expected_stdout.rstrip():
        cprint("unexpected stdout", "red", attrs=["bold"])
        print("expected stdout ---------------------")
        print(expected_stdout)
        print("stdout ------------------------------")
        print(stdout)
        print("stderr ------------------------------")
        print(stderr)
        return

    if disable_output_check == False and stderr.rstrip() != expected_stderr.rstrip():
        cprint("unexpected stderr", "red", attrs=["bold"])
        print("expected stderr ---------------------")
        print(expected_stderr)
        print("stdout ------------------------------")
        print(stdout)
        print("stderr ------------------------------")
        print(stderr)
        return

    if returncode != expected_returncode:
        cprint(f"exited with {returncode}", "red", attrs=["bold"])
        return

    cprint("ok", "green", attrs=["bold"])

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("tests", nargs="*")
    args = parser.parse_args()

    if len(args.tests) > 0:
        tests = args.tests
    else:
        tests = Path("test").glob("**/*.sh")

    subprocess.run(["cargo", "build"], check=True)

    for test in tests:
        run_test(test)

if __name__ == "__main__":
    main()
