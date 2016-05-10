# Bottom-Up Dynamic Programming Algorithm {#sec:dynamic-narrative}

The DP implementation uses bottom-up iterative approach in `Fig. 15.8` in
[@Cormen2009].

For the `Python` implementation, see listing in @sec:dynamic-listing.

As with _memoized algorithm_, we expect $\Theta(mn)$ running time and memory requirements for the task of sizing an LCS.
Also, we expect linear time $O(m + n)$ and quadratic space $O(mn)$ for reconstructing an LCS, given the table
computed beforehand.

As the plots in @sec:summary demonstrate, the _dynamic algorithm_ has indeed polynomial
execution time and memory performance for sizing an LCS, and linear time
for reconstructing an LCS.

It will be seen in @sec:summary that the _dynamic algorithm_ implementation
is more efficient than _memoized algorithm_ because of the recursive
overhead of the latter. However, my table storage implementation for the two
algorithms differs, in that the table for the _dynamic algorithm_ just happens
to be less efficiently implemented.
This results in the _dynamic algorithm_ requiring significantly more memory
for the same input length, compared to my implementation of the _memoized algorithm_.
Again, this is a mere fluke of implementation and not in any way intrinsic in
the algorithms themselves.


<!--

![CPU time vs input string length: memoization vs dynamic algorithms. \label{cpu-vs-len-compare-algos}](source/figures/CPU_vs_input_length__memoized_vs_dynamic.ps)

-->


Figure \ref{cpu-vs-len-dynamic} shows a run time profiler
output for a string of size 10,000.

<!--

![CPU time for a long input string: dynamic algorithm. \label{cpu-vs-len-dynamic}](source/figures/CPU_vs_input_length__dynamic_only.ps)

-->
