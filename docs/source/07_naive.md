# Naive Algorithm {#sec:naive-narrative}

The naive recursive solution is based on recursion 
$(15.9)$ in [@Cormen2009] repeated here for clarity.

\begin{equation}
    c[i, j] = 
    \begin{cases}
        0, & \text{if } i=0 \text{ or } j=0, \\
        c[i-1, j-1]+1 & \text{if } i,j>0 \text{ and } x_i = y_i, \\
        max(c[i, j-1], c[i-1,j]) & \text{if } i,j>0 \text{ and } x_i \neq y_j
    \end{cases}
\end{equation}

For the `Python` implementation, see listing in @sec:naive-listing.

Figure \ref{cpu-vs-len-naive} shows a run time profiler results using the 
naive algorithm for strings of varying length and structure. 
For a feasible run time, only
strings of length $10$ and $15$ were used. Both binary and character
alphabets are compared.

![CPU time vs input string length: naive algorithm. \label{cpu-vs-len-naive}](source/figures/CPU_vs_string_length__naive_algorithm.ps)

Figure \ref{cpu-vs-recursion-naive} shows a recursion depth profiler run 
using the naive algorithm and character alphabet
for strings of varying length and structure. For a feasible run time, only
strings of length $10$ and $15$ were used.

![CPU time vs recursion depth: naive algorithm. \label{cpu-vs-recursion-naive}](source/figures/CPU_vs_recursion_depth.ps)
