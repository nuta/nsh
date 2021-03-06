#!/usr/bin/env python3
#
#  The blackbox test runner. Write this script in Python 3.5 to run
#  on Travis CI.
#
import argparse
import os
import sys
import subprocess
from pathlib import Path
from termcolor import cprint
import re

def run_test(build, test):
    cprint("Running {}...".format(test), attrs=["bold"], end="")
    sys.stdout.flush()

    expected_stdout_path = Path(test).with_suffix(".stdout")
    expected_stderr_path = Path(test).with_suffix(".stderr")

    try:
        expected_stdout = expected_stdout_path.open().read()
    except FileNotFoundError:
        expected_stdout = ""

    try:
        expected_stderr = expected_stderr_path.open().read()
    except FileNotFoundError:
        expected_stderr = ""

    test_body = Path(test).open().read()
    disable_output_check = "disable-output-check" in test_body

    m = re.search(r"exit-with=([0-9]+)", test_body)
    if m is None:
        expected_returncode = 0
    else:
        expected_returncode = int(m.groups()[0])

    # Before running nsh, make sure that bash outputs expected stdout.
    p = subprocess.Popen(["bash", str(test)], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    bash_returncode = p.wait()
    bash_stdout = p.stdout.read().decode("utf-8")
    bash_stderr = p.stderr.read().decode("utf-8")

    if bash_returncode != expected_returncode:
        cprint("bash returned {} (expected {})".format(bash_returncode, expected_returncode), "red", attrs=["bold"])
        print("bash stdout -------------------------")
        print(bash_stdout)
        print("bash stderr -------------------------")
        print(bash_stderr)
        return False

    if disable_output_check == False and bash_stdout.rstrip() != expected_stdout.rstrip():
        cprint("unexpected bash output (fix {}!)".format(expected_stdout_path), "red", attrs=["bold"])
        print("expected ----------------------------")
        print(expected_stdout)
        print("bash stdout -------------------------")
        print(bash_stdout)
        print("bash stderr -------------------------")
        print(bash_stderr)
        return False

    # Run the test.
    p = subprocess.Popen(
        ["./target/{}/nsh".format(build), "--norc", str(test)],
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
        return False

    if disable_output_check == False and stderr.rstrip() != expected_stderr.rstrip():
        cprint("unexpected stderr", "red", attrs=["bold"])
        print("expected stderr ---------------------")
        print(expected_stderr)
        print("stdout ------------------------------")
        print(stdout)
        print("stderr ------------------------------")
        print(stderr)
        return False

    if returncode != expected_returncode:
        cprint("exited with {}".format(returncode), "red", attrs=["bold"])
        return False

    cprint("ok", "green", attrs=["bold"])
    return True

def generate_test_files():
    FILES = [
        "friends/songs/ross_can.mp3",
        "friends/songs/smelly_cat.mp3",
        "friends/the_one_with_the_lottery.txt",
        "monty_python/cheese.txt",
        "monty_python/parrot.txt",
        "monty_python/spam_egg_spam_spam_bacon_and_span.txt",
        "monty_python/spam_spam_spam_spam_spam_spam_baked_beans_spam_spam_spam_and_spam.txt",
    ]

    for f in FILES:
        path = Path("test/files") / f
        os.makedirs(str(path.parent), exist_ok=True)
        path.write_text("dummy file for testing")

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--release", action="store_true")
    parser.add_argument("tests", nargs="*")
    args = parser.parse_args()

    if len(args.tests) > 0:
        tests = args.tests
    else:
        tests = Path("test").glob("**/*.sh")

    if args.release:
        build = "release"
        subprocess.run(["cargo", "build", "--release"], check=True)
    else:
        build = "debug"
        subprocess.run(["cargo", "build"], check=True)

    generate_test_files()

    num_failed = 0
    for test in tests:
        if not run_test(build, test):
            num_failed += 1

    if num_failed > 0:
        cprint("{} tests failed".format(num_failed), "red", attrs=["bold"])
        sys.exit(1)

if __name__ == "__main__":
    main()
