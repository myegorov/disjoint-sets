# Top-Down Memoized Algorithm {#sec:memoized-narrative}

The memoized implementation uses top-down recursion essentially identical
to the naive approach in @sec:naive-listing, except that performed
computations are saved in a table.

For the `Python` implementation, see listing in @sec:memoized-listing.

We expect $\Theta(mn)$ running time and memory requirements for the task of sizing an LCS.
Also, we expect linear time $O(m + n)$ and quadratic space $O(mn)$ for reconstructing an LCS, given the table
computed beforehand.

It will be seen in @sec:summary that the _memoized algorithm_ has indeed polynomial
execution time and memory performance for sizing an LCS, and linear time
for reconstructing an LCS.
