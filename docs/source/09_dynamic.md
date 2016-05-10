# Bottom-Up Dynamic Programming Algorithm {#sec:dynamic-narrative}

The DP implementation uses bottom-up iterative approach in `Fig. 15.8` in
[@Cormen2009]. For the `Python` implementation, see listing in @sec:dynamic-listing.

As with the _memoized algorithm_, we expect $\Theta(mn)$ running time and memory requirements for the task of sizing an LCS.
Also, we expect linear time $\Theta(m + n)$ and quadratic space $\Theta(mn)$ for reconstructing an LCS, given the table
computed beforehand.

As the plots in @sec:summary demonstrate, the _dynamic algorithm_ has indeed quadratic
execution time and memory performance for sizing an LCS, and linear time
for reconstructing an LCS.

It will be seen from the plots in @sec:summary that the _dynamic algorithm_ implementation
is more efficient than _memoized algorithm_ because of the recursive
overhead of the latter. However, my table storage implementation for the two
algorithms is differenty (by accident). The table for the _dynamic algorithm_ just happens
to be less efficiently implemented.
This results in the _dynamic algorithm_ requiring significantly more memory
for the same input length, compared to my implementation of the _memoized algorithm_.
Again, this is a mere fluke of implementation and not in any way intrinsic in
the algorithms themselves. I will comment on the particular plots that
illustrate this fluke further in @sec:summary.


For set 2 we can approximate the distribution of CPU times for sizing an LCS
for both alphabetic and binary strings: $CPU \, time = 5.79 \times 10^{-5} \;  x^{2}$;

Correspondingly, for a 10 second runtime, we could process inputs up to
about size 420 characters, which approximately matches the performance of
the _memoized_ algorithm.

