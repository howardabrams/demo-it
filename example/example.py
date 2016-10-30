# This Python program is a simple example.
import sys


def greeting(name):
    """Prints a greeting when given the
       optional name variable."""
    if name:
        print "Hello, %s" % name
    else:
        print "Hello World"


if __name__ == "__main__":
    if len(sys.argv) > 0:
        name = sys.argv[1]
    else:
        name = None

    greeting(name)
