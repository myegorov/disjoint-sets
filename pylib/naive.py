#!/usr/bin/env python3

"""
naive.py

Naive recursive solution to longest common subsequence problem.
See naive.py_bak for example of memory profiling.

Usage:
    python3 naive.py
"""

__author__ = "Maksim Yegorov"
__date__ = "2016-05-01 Sun 06:49 PM"


from profilers import log_recursion
from profilers import time_and_space_profiler
from profilers import registry
from generate_string import strgen
import sys

sys.setrecursionlimit(100000)


@time_and_space_profiler(repeat = 1)
def reconstruct_lcs(seq1, seq2, *args):
    """Calls helper function to calculate an LCS.

    Args:
        *args:  extra arguments that some algorithms
                require
        """
    # reset registry
    registry['_reconstruct_lcs'] = 0

    return _reconstruct_lcs(seq1, seq2, len(seq1)-1, \
                len(seq2)-1, "")

@log_recursion
def _reconstruct_lcs(seq1, seq2, i, j, lcs):
    """Naive recursive solution to LCS problem.
    See CLRS pp.392-393 for the recursive formula.

    Args:
        seq1 (string):  a string sequence generated
                        by generate_string.strgen()
        seq2 (string):  another random string
                            sequence like seq1
        i (int):        index into seq1
        j (int):        index into seq2
        lcs (string):   an LCS string being built-up
    Returns:
        lcs:    longest common subsequence (can be
                    empty string)
    """

    if i < 0 or j < 0:
        return lcs
    else:
        if seq1[i] == seq2[j]:
            return _reconstruct_lcs(seq1, seq2, \
                    i-1, j-1, seq1[i] + lcs)
        else:
            return max(_reconstruct_lcs(seq1, \
                            seq2, i-1, j, lcs),
                    _reconstruct_lcs(seq1, \
                            seq2, i, j-1, lcs), \
                            key=len)


if __name__ == "__main__":
    """Tests and top-level logic go here."""

    sequence_1 = strgen(['a','b','c'], 10)
    sequence_2 = strgen(['a','b','c'], 10)
    print("seq1: %s" %sequence_1)
    print("seq2: %s" %sequence_2)

    func_name, elapsed, memlog, lcs = reconstruct_lcs(sequence_1, sequence_2)
    print("LCS length: %d" %len(lcs))
    print("LCS: %s" %lcs)
    print("[%0.7fs] %s(%d) -> %d recursive calls"
            %(elapsed, func_name, len(sequence_1), \
                    registry['_reconstruct_lcs']))

    # test reconstruction match
    waste, waste, memlog, lcs = reconstruct_lcs("","")
    assert lcs == ""
    waste, waste, memlog, lcs = reconstruct_lcs("", "123")
    assert lcs == ""
    waste, waste, memlog, lcs = reconstruct_lcs("123", "")
    assert lcs == ""
    waste, waste, memlog, lcs = reconstruct_lcs("123", "abc")
    assert lcs == ""
    waste, waste, memlog, lcs = reconstruct_lcs("123", "123")
    assert lcs == "123"
    waste, waste, memlog, lcs = reconstruct_lcs("bbcaba", "cbbbaab")
    assert lcs == "bbab"
