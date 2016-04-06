#!/usr/bin/env python3

from random import choice

def strgen(alphabet=['0', '1'], size=40000):
    """Generates string of characters from alphabet of given length."""
    astring = ""
    for i in range(size):
        astring += choice(alphabet)
    return astring

if __name__ == "__main__":
    # functionality test
    for i in range(5):
        some_string = strgen(['A','C','G','T'], 3)
        print("Generated: %s of length %d" %(some_string, len(some_string)))
