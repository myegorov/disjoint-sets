 #!/usr/bin/env python3

"""
memoized.py

Recursive solution with memoization to longest common subsequence problem.

Usage:
    python3 memoized.py
"""

__author__ = "Maksim Yegorov"
__date__ = "2016-05-06 Fri 12:58 AM"

from profilers import log_recursion, time_and_space_profiler
from profilers import registry
from generate_string import strgen
import sys

# set system recursion limit
sys.setrecursionlimit(100000)


@time_and_space_profiler(repeat = 1)
def tabulate_lcs(seq1, seq2, *args):
    """Calls helper function to calculate an LCS.

    Args:
        seq1 (string):  a random string sequence
                            generated by generate_string.strgen()
        seq2 (string):  another random string sequence like seq1
    Returns:
        table of LCS lengths (int): so-called table c
                in Figure 15.8 in CLRS

    """
    # reset registry
    registry['_tabulate_lcs'] = 0

    len1 = len(seq1)
    len2 = len(seq2)

    # store length of LCS[i,j] in lcs_table
    lcs_table = [[None for j in range(len2)] \
                    for i in range(len1)]
    _tabulate_lcs(seq1, seq2, len1-1, len2-1, \
                    lcs_table)
    #return lcs_table[len1-1][len2-1]
    return lcs_table

@log_recursion
def _tabulate_lcs(seq1, seq2, i, j, lcs_table):
    """Recursive solution with memoization to LCS problem.
    See CLRS ex. 15.4-3.

    Args:
        seq1 (string):  a string sequence generated by
                            generate_string.strgen()
        seq2 (string):  another random string sequence like seq1
        i (int):        index into seq1
        j (int):        index into seq2
        lcs_table (2D list):   a matrix of LCS length for
                                [i, j] prefix
    Returns:
        None:    modifies in place LCS length table
    """

    if i < 0 or j < 0:
        return 0
    else:
        if lcs_table[i][j] is not None:
            return lcs_table[i][j]
        else:
            if seq1[i] == seq2[j]:
                val = 1 + \
                    _tabulate_lcs(seq1, seq2, i-1, \
                                    j-1, lcs_table)
            else:
                val = max(_tabulate_lcs(seq1, seq2, \
                                    i-1, j, lcs_table),
                        _tabulate_lcs(seq1, seq2, i, \
                                    j-1, lcs_table))

            lcs_table[i][j] = val
            return val

def size_lcs(lcs_table):
    """Returns length of maximum common subsequence.

    Args:
        lcs_table (2D list):   a matrix of LCS length for
                                [i, j] prefix
    Returns:
        length (int):           LCS length
    """
    if len(lcs_table) > 0 and len(lcs_table[0]) > 0:
        return lcs_table[-1][-1]
    else:
        return 0

@time_and_space_profiler(repeat = 1)#, stream = MEMLOG)
def reconstruct_lcs(seq1, seq2, lcs_table, lcs_length):
    """Calls helper function to reconstruct
    one possible LCS based on saved LCS lengths table.

    Args:
        seq1 (string):  a string sequence generated by
                            generate_string.strgen()
        seq2 (string):  another random string sequence like seq1
        lcs_length (int):   length of LCS
        lcs_table (2D list):   a matrix of LCS length for
                                [i, j] prefix
    Returns:
        lcs (string):   an LCS
    """

    # reset registry
    registry['_reconstruct_lcs'] = 0

    i = len(lcs_table) - 1
    if i < 0:
        return ""
    else:
        j = len(lcs_table[0]) - 1
        lcs_arr = _reconstruct_lcs(seq1, seq2, lcs_table,
                lcs_length-1, i, j, [None] * lcs_length)
        lcs = "".join(lcs_arr)
        return lcs

@log_recursion
def _reconstruct_lcs(seq1, seq2, lcs_table, char, i, j,\
        lcs_arr):

    # if already constructed LCS, return
    if (char < 0 or i < 0 or j < 0):
        return lcs_arr
    # else if looking for first character of LCS...
    elif (i == 0):
        if (lcs_table[i][j] == 1):
            if (seq1[i] == seq2[j]):
                lcs_arr[char] = seq1[i]
                return lcs_arr
            else:
                return _reconstruct_lcs(seq1, seq2, lcs_table,
                    char, i, j-1, lcs_arr)
        else:
            return lcs_arr
    elif (j == 0):
        if (lcs_table[i][j] == 1):
            if (seq1[i] == seq2[j]):
                lcs_arr[char] = seq1[i]
                return lcs_arr
            else:
                return _reconstruct_lcs(seq1, seq2, lcs_table,
                    char, i-1, j, lcs_arr)
        else:
            return lcs_arr
    # else consider general case
    else:
        prev, up, left = (lcs_table[i-1][j-1],
                            lcs_table[i-1][j],
                            lcs_table[i][j-1])

        if (seq1[i] == seq2[j]):
            lcs_arr[char] = seq1[i]
            return _reconstruct_lcs(seq1, seq2, lcs_table,
                    char-1, i-1, j-1, lcs_arr)

        elif (left is not None and up is not None):
            if lcs_table[i-1][j] > lcs_table[i][j-1]:
                return _reconstruct_lcs(seq1, seq2, lcs_table,
                    char, i-1, j, lcs_arr)
            else:
                return _reconstruct_lcs(seq1, seq2, lcs_table,
                    char, i, j-1, lcs_arr)
        elif (left is not None):
            return _reconstruct_lcs(seq1, seq2, lcs_table,
                    char, i, j-1, lcs_arr)
        else:
            return _reconstruct_lcs(seq1, seq2, lcs_table,
                    char, i-1, j, lcs_arr)



if __name__ == "__main__":
    """Tests and top-level logic go here."""

    sequence_1 = strgen(['a','b','c'], 1000)
    sequence_2 = strgen(['a','b','c'], 1000)
    #sequence_1 = "aaabb"
    #sequence_2 = "abbba"
    print("seq1: %s" %sequence_1)
    print("seq2: %s" %sequence_2)

    name, elapsed, memlog, lcs_table = \
            tabulate_lcs(sequence_1, sequence_2)
    lcs_length = size_lcs(lcs_table)
    recursion_depth = registry['_tabulate_lcs']

    print("LCS length: %d" %lcs_length)
    print("name: " + name)
    print("runtime: " + str(elapsed))
    print("recursion_depth: " + str(recursion_depth))

    #for row in lcs_table:
    #    print(row)
    #print()

    name, elapsed, memlog, lcs = \
            reconstruct_lcs(sequence_1,
                            sequence_2,
                            lcs_table,
                            lcs_length)
    print("\n(an) LCS: %s" %lcs)
    print("[%0.7fs] %s(%d) -> %d recursive calls"
            %(elapsed, name, lcs_length, \
                registry['_reconstruct_lcs']))

    # test reconstruction match
    name, elapsed, memlog, lcs_table = \
            tabulate_lcs("","")
    lcs_length = size_lcs(lcs_table)
    waste, waste, memlog, lcs = reconstruct_lcs("", "", lcs_table, lcs_length)
    assert lcs == ""
    name, elapsed, memlog, lcs_table = \
            tabulate_lcs("","123")
    lcs_length = size_lcs(lcs_table)
    waste, waste, memlog, lcs = reconstruct_lcs("", "123", lcs_table, lcs_length)
    assert lcs == ""
    name, elapsed, memlog, lcs_table = \
            tabulate_lcs("123","")
    lcs_length = size_lcs(lcs_table)
    waste, waste, memlog, lcs = reconstruct_lcs("123", "", lcs_table, lcs_length)
    assert lcs == ""
    name, elapsed, memlog, lcs_table = \
            tabulate_lcs("123","abc")
    lcs_length = size_lcs(lcs_table)
    waste, waste, memlog, lcs = reconstruct_lcs("123", "abc", lcs_table, lcs_length)
    assert lcs == ""
    name, elapsed, memlog, lcs_table = \
            tabulate_lcs("123","123")
    lcs_length = size_lcs(lcs_table)
    waste, waste, memlog, lcs = reconstruct_lcs("123", "123", lcs_table, lcs_length)
    assert lcs == "123"
    name, elapsed, memlog, lcs_table = \
            tabulate_lcs("bbcaba","cbbbaab")
    lcs_length = size_lcs(lcs_table)
    waste, waste, memlog, lcs = reconstruct_lcs("bbcaba", "cbbbaab", lcs_table, lcs_length)
    assert lcs == "bbba"
