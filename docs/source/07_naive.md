# Naive Algorithm {#sec:naive-narrative}

The naive recursive solution is based on recursion
$(15.9)$ in [@Cormen2009]. I repeat the recursion here as it is of
fundamental importance for all the algorithms discussed in this report.
For the `Python` implementation, see listing in @sec:naive-listing.

\begin{equation}
    c[i, j] =
    \begin{cases}
        0, & \text{if } i=0 \text{ or } j=0, \\
        c[i-1, j-1]+1 & \text{if } i,j>0 \text{ and } x_i = y_i, \\
        max(c[i, j-1], c[i-1,j]) & \text{if } i,j>0 \text{ and } x_i \neq y_j
    \end{cases}
\end{equation}


strings of length up to $20$ were tested. Asymptotic complexity of the _naive recursive algorithm_
is exponential in
the length of the input strings. Both the length and the actual LCS match
are computed at once.
The asymptotic time is confirmed by the experimental results shown in @sec:summary,
where the performance of the _naive algorithm_ is orders of magnitude worse
than that of any of the quadratic/linear time algorithms.

The stark difference in performance is most clearly seen in set 2 plots (runtime vs input and recursion depth vs input)
in  @sec:summary and correlates strongly with the recursion depth, but is
further compounded by the repeated re-calculation of the same quantities.


We could fit an exponential curve to the $O(c^n)$ data distribution to
approximate the constant $c$, under the assumption that all lower-order
terms are negligible. However, the added precision is
not very useful as the numbers will differ, sometimes dramatically, even between
different batch runs, to say nothing about different machines. Compare
the runtime for strings of size 20 (binary alphabet) for the two sets.

To illustrate, for set 1 we can approximate the distribution with:

1) for alphabetic strings: $CPU \, time = 7.3 \times 10^{-5} \; e^{0.83n}$;
2) for binary strings: $CPU \, time = 2.5 \times 10^{-3} \; e^{0.25n}$;

For set 2:

1) for alphabetic strings: $CPU \, time = 1.8 \times 10^{-4} \;  e^{0.80n}$;
2) for binary strings: $CPU \, time = 1.5 \times 10^{-3} \; e^{0.29n}$;

With the above dislaimer about the approximate nature of any prediction
(specific to machine and input characteristics),
we can estimate that for 10 second execution time, we can process
at input strings of at most length 13 if using DNA alphabet.

