# Top-Down Memoized Algorithm {#sec:memoized-narrative}

The memoized implementation uses top-down recursion essentially identical
to the naive approach in @sec:naive-listing, except that performed
computations are saved in a table to eliminate repeated superfluous
calculations. For the `Python` implementation, see listing in @sec:memoized-listing.

We expect $\Theta(mn)$ running time and memory requirements for the task of sizing an LCS.
Also, we expect linear time $\Theta(m + n)$ and quadratic space $\Theta(mn)$ for reconstructing an LCS, given the table
computed beforehand.

It will be seen in @sec:summary that the _memoized algorithm_ has indeed quadratic
execution time and memory performance for sizing an LCS, and linear time
for reconstructing an LCS. See, in particular the runtime and memory plots
from set 2 in @sec:summary.


For set 2 we can approximate the distribution of CPU times for sizing an LCS
with:

1) for alphabetic strings: $CPU \, time = 7.28 \times 10^{-5} \;  x^{2}$;
2) for binary strings: $CPU \, time = 1.75 \times 10^{-8} \; x^{2}$;

Correspondingly, for a 10 second runtime, we could process inputs up to
about size 460 characters.

