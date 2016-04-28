 #!/usr/bin/env python3

"""
hirschberg.py

Iterative bottom-up dynamic programming solution to longest common
subsequence problem with Hirschberg's contributions:
    (1) reduce length matrix to vector;
    (2) reduce to linear size storage.

Usage:
    python3 hirschberg.py
"""

__author__ = "Maksim Yegorov"
__date__ = "2016-04-28 Thu 02:52 PM"

from profilers import log_recursion, time_and_space_profiler
from profilers import registry, MEMLOG
from generate_string import strgen
import sys

sys.setrecursionlimit(10000)

@time_and_space_profiler(repeat = 1, stream = MEMLOG)
def lcs_hirschberg(seq1, seq2):
    """Calls helper function to calculate an LCS.

    Args:
        seq1 (string):  a random string sequence generated by
                        generate_string.strgen()
        seq2 (string):  another random string sequence like seq1
    Returns:
        LCS table

    """
    len1 = len(seq1)
    len2 = len(seq2)

    # for efficiency (see ALG B)
    # select min(|seq1|, |seq2|) for vector storage
    if len1 < len2:
        seq1, seq2 = seq2, seq1
        len1, len2 = len2, len1

    # if only the length of the LCS is required,
    # the matrix can be reduced to a min(m,n)+1 vector
    # as the dynamic programming approach only needs the
    # current and previous columns of the matrix.
    lcs_vector = [0 for j in range(len2+1)]
    _lcs_hirschberg(seq1, seq2, len1+1, len2+1,
                    lcs_vector)
    return lcs_vector

def _lcs_hirschberg(seq1, seq2, i, j, lcs_vector):
    """Iterative Hirschberg dynamic programming solution to
    LCS problem. See Hirschberg's ALG B.

    Args:
        seq1 (string):  a string sequence generated by
                            generate_string.strgen()
        seq2 (string):  another random string sequence
                            like seq1
        i (int):        number of rows in LCS table
                            (=len(seq1) + 1)
        j (int):        number of columns in LCS table
                            (=len(seq2) + 1)
        lcs_vector (1D list):   a vector of LCS length for
                                [i-1, j-1] prefix, as in ALG B
    Returns:
        None:    modifies in place LCS length table
    """

    for char in range(1, i):
        prev = 0
        for col in range(1, j):
            if seq1[char-1] == seq2[col-1]:
                tmp = prev
                prev = lcs_vector[col-1] + 1
                lcs_vector[col-1] = tmp
            elif lcs_vector[col] >= prev:
                lcs_vector[col-1] = prev
                prev = lcs_vector[col]
            else:
                lcs_vector[col-1] = prev
                # prev = prev
            if col == j - 1:
                lcs_vector[col] = prev

@time_and_space_profiler(repeat = 1, stream = MEMLOG)
def algC(seq1, seq2):
    """Calls helper function to construct LCS."""

    m = len(seq1)
    n = len(seq2)

    if (m == 0 or n == 0):
        return ""

    # for efficiency (see ALG B)
    # select min(|seq1|, |seq2|) for vector storage
    if m < n:
        seq1, seq2 = seq2, seq1
        m, n = n, m

    lcs_arr = _algC(m, n, seq1, seq2)

    lcs = "".join(lcs_arr)
    return lcs

@log_recursion
def _algC(m, n, seq1, seq2):
    """Implements Algorithm C by Hirschberg.

    Args:
        seq1 (str): sequence 1
        seq2 (str): sequence 2
        m (int): length of seq1
        n (int): length of seq2
    """
    if n == 0:
        return []
    elif m == 1:
        if seq1[0] in seq2:
            return [seq1[0]]
        else:
            return []
    else:
        mid = m // 2

        lcs_vector_1 = [0 for j in range(n+1)]
        lcs_vector_2 = [0 for j in range(n+1)]

        _lcs_hirschberg(seq1[:mid], seq2, mid+1, n+1, lcs_vector_1)
        _lcs_hirschberg(seq1[:mid-1:-1], seq2[::-1],
                m-mid+1, n+1, lcs_vector_2)

        sums = [lcs_vector_1[i] + lcs_vector_2[n-i] for i in \
                range(len(lcs_vector_1))]

        k = sums.index(max(sums))

        C1 = _algC(mid, min(k, n), seq1[:mid], seq2[:k])
        C2 = _algC(m-mid, n-min(k, n), seq1[mid:], seq2[k:])
        C1.extend(C2)
        return C1

def size_lcs(lcs_vector):
    """Returns length of maximum common subsequence.

    Args:
        lcs_vector (1D list):   a vector of LCS length for
                                [i, j] prefix
    Returns:
        length (int):           LCS length
    """
    return lcs_vector[-1]


if __name__ == "__main__":
    """Tests and top-level logic go here."""

    sequence_1 = strgen(['a','b','c'], 100)
    sequence_2 = strgen(['a','b','c'], 100)
    #print("seq1: %s" %sequence_1)
    #print("seq2: %s" %sequence_2)

    # calculate length of LCS
    name, elapsed, lcs_vector = \
            lcs_hirschberg(sequence_1, sequence_2)
    lcs_length = size_lcs(lcs_vector)
    print("LCS length: %d" %lcs_length)

    # reconstruct LCS
    name, elapsed, lcs = algC(sequence_1, sequence_2)
    print("\n(an) LCS: ", lcs)
    print("[%0.7fs] %s(%d) -> %d recursive calls"
            %(elapsed, name, lcs_length, \
                    registry['_algC']))

    # test reconstruction match
    waste, waste, lcs = algC("","")
    assert lcs == ""
    waste, waste, lcs = algC("", "123")
    assert lcs == ""
    waste, waste, lcs = algC("123", "")
    assert lcs == ""
    waste, waste, lcs = algC("123", "abc")
    assert lcs == ""
    waste, waste, lcs = algC("123", "123")
    assert lcs == "123"
    waste, waste, lcs = algC("bbcaba", "cbbbaab")
    assert lcs == "bbab"
