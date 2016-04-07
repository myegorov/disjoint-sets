# Bottom-Up Dynamic Programming Algorithm {#sec:dynamic-narrative}

The DP implementation uses bottom-up iterative approach in `Fig. 15.8` in 
[@Cormen2009]. *TODO*: add pseudo-code.

For the `Python` implementation, see listing in @sec:dynamic-listing.

Figure \ref{cpu-vs-len-compare-algos} shows a run time profiler output
comparing the bottom-up dynamic vs top-down memoization algorithm performance
for the same string input.

![CPU time vs input string length: memoization vs dynamic algorithms. \label{cpu-vs-len-compare-algos}](source/figures/CPU_vs_input_length__memoized_vs_dynamic.ps)


Figure \ref{cpu-vs-len-dynamic} shows a run time profiler 
output for a string of size 10,000.

![CPU time for a long input string: dynamic algorithm. \label{cpu-vs-len-dynamic}](source/figures/CPU_vs_input_length__dynamic_only.ps)
