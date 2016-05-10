# Hirschberg Linear Space Dynamic Programming Algorithm {#sec:hirschberg-narrative}

The _Hirschberg algorithm_ implementation follows the pseudo-code in [@Hirschberg1975].

For the `Python` implementation, see listing in @sec:hirschberg-listing.

Theoretically, we expect $\Theta(mn)$ time complexity and $\Theta(m + n)$ space.
By distinction from the _memoized_ and _dynamic_ algorithms that require
quadratic ($\Theta(mn)$ space for recovery, not just sizing an LCS), _Hirschberg_ algorithm
allows one also to recover
an LCS in $\Theta(m + n)$ space. However, also by contrast to the _memoized_ and
_dynamic_ algorithms, _Hirschberg_ requires a $\Theta(mn)$ time to recover an LCS,
where the former two algorithms are linear $Theta(m + n)$. I.e. where the
former two algorithms excel in the time requirements for recovery, _Hirschberg_
excels in the space requirements for recovery.

The linear space requirements and polynomial time requirements will indeed be evident in the plots in
@sec:summary.
