# Hirschberg Linear Space Dynamic Programming Algorithm {#sec:hirschberg-narrative}

The _Hirschberg algorithm_ implementation follows the pseudo-code in [@Hirschberg1975].
For the `Python` implementation, see listing in @sec:hirschberg-listing.

Theoretically, we expect $\Theta(mn)$ time complexity and $\Theta(m + n)$ space.
By distinction from the _memoized_ and _dynamic_ algorithms that require
quadratic ($\Theta(mn)$ space for recovery, not just sizing an LCS), _Hirschberg_ algorithm
allows one also to recover
an LCS in $\Theta(m + n)$ space. However, also by contrast to the _memoized_ and
_dynamic_ algorithms, _Hirschberg_ requires a $\Theta(mn)$ time to recover an LCS,
where the former two algorithms are linear $\Theta(m + n)$. I.e. in the tradeoff
between time and memory consumption -- the
former two algorithms excel in the time requirements (for recovering an LCS), while _Hirschberg_
excels in the space requirements (similarly for recovering an LCS).

The linear space requirements and polynomial time requirements will indeed be evident in the plots in
@sec:summary.

For set 2 we can approximate the distribution of CPU times for sizing an LCS
with:

1) for alphabetic strings: $CPU \, time = 5.09 \times 10^{-6} \;  x^{2}$;
2) for binary strings: $CPU \, time = 5.15 \times 10^{-6} \; x^{2}$;

Correspondingly, for a 10 second runtime, we could process inputs up to
about size 1350 characters, almost three times the performance of the
_memoized_ or _dynamic_ algorithms.

