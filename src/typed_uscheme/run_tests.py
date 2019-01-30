"""Script for running `uscheme` integration tests.

Simply runs the `uscheme` interpreter and checks for uscheme unit test failures.
"""

from subprocess import Popen, PIPE
import os
import re
import subprocess
import sys
import textwrap

DIR = os.path.dirname(__file__)
SCHEME_PATH = os.path.join(DIR, './uscheme')

def abs_of_base_path(fname):
    return os.path.join(DIR, "tests", fname)

def fetch_test_names():
    return os.listdir("tests")

def indent(s):
    return textwrap.indent(s, ' ' * 4)

def run_test(name):
    p = Popen(SCHEME_PATH, stdin=PIPE, stdout=PIPE, stderr=PIPE)

    timeout = False
    try:
        inpt_string = f"(use {abs_of_base_path(name)})\n"
        stdout, stderr = p.communicate(bytes(inpt_string, encoding="UTF-8"),
                                       timeout=10);
        stdout = stdout.decode()
        stderr = stderr.decode()
    except subprocess.TimeoutExpired:
        timeout = True

    if timeout:
        print(f"{name} timed out")
        print(f"captured stdout:\n\n{indent(stdout)}\n")
        print(f"captured stderr:\n\n{indent(stderr)}\n")
        return False

    pass_regex = re.compile(r"(\d+) of (\d+) unit tests passed")

    match = pass_regex.search(stdout)

    if match is None:
        print(f"{name} failed--unit tests did not run")
        print(f"captured stdout:\n\n{indent(stdout)}\n")
        print(f"captured stderr:\n\n{indent(stderr)}\n")
        return False

    if match[1] != match[2]:
        print(f"{name} failed--not all tests passed")
        print(f"captured stdout:\n\n{indent(stdout)}\n")
        print(f"captured stderr:\n\n{indent(stderr)}\n")
        return False

    print(f"{name}: passed")
    return True

def main():
    tests = fetch_test_names()
    count = 0

    for test in tests:
        count += run_test(test)

    print(f"Tests complete.\n{count}/{len(tests)} tests passed.")

    if count != len(tests):
        sys.exit(1)

if __name__ == "__main__":
    main()
