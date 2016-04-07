#!/usr/bin/env python3
"""
generate_string.py

Generate a string given alphabet and length of string.

Usage:
    python3 generate_string.py
"""

__author__ = "Maksim Yegorov"
__date__ = "2016-04-06 Wed 08:06 PM"

from random import choice

def strgen(alphabet=['0', '1'], size=40000):
    """Generates string of characters from 
    alphabet of given length."""
    astring = ""
    for i in range(size):
        astring += choice(alphabet)
    return astring

if __name__ == "__main__":
    # functionality test
    for i in range(5):
        some_string = \
                strgen(['A','C','G','T'], 3)
        print("Generated: %s of length %d" \
                %(some_string, len(some_string)))
