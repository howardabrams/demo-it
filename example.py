# This Python program is a simple example.
import sys


def world(name):
    "Prints a greeting when given the optional name variable"
    if name:
        print "Hello, %s" % name
    else:
        print "Hello World"


if __name__ == "__main__":
    world(sys.argv[1])
