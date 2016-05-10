# Introduction

In this report, I implement and investigate the performance of four algorithms that
each calculate the length of and reconstruct a longest
subsequences common to a pair of input strings. The algorithms are -- in the order of
increasing sophistication -- the naive recursive, top-down memoized recursive,
bottom-up dynamic iterative, and Hirschberg's quadratic time linear space
recursive algorithms. The implementation of all algorithms except Hirschberg's quadratic-time linear-space algorithm is based on [@Cormen2009].
For Hirschberg's Algorithm B and Algorithm C, see [@Hirschberg1975].

This is an empirical investigation of the actual runtime performance. The
algorithms where implemented using the [`Python`](https://www.python.org/)
programming language. `Python` is a high-level interpreted language. The
reason that I chose `Python` is that it offers a near pseudocode-level
clarity of the implementation. The drawback is a comparatively
long execution time.
For this exercise where we merely compare the algorithms
among themselves -- without worrying about putting them in production --
`Python` proved to be an adequate choice, especially from the standpoint of rapidly
coming up with a prototype implementation.

The algorithms were run in two batches remotely on a CS department lab machine (`gorgon.cs.rit.edu`)
with the following characteristics:

```shell
$ cat /proc/cpuinfo
processor       : 0
vendor_id       : GenuineIntel
cpu family      : 6
model           : 42
model name      : Intel(R) Core(TM) i5-2400 CPU @ 3.10GHz
stepping        : 7
microcode       : 0x1b
cpu MHz         : 1600.000
cache size      : 6144 KB
physical id     : 0
siblings        : 4
core id         : 0
cpu cores       : 4
   ...

$ cat /proc/meminfo
MemTotal:       16391732 kB
MemFree:        12982560 kB
Buffers:          485152 kB
Cached:          1695260 kB
SwapCached:            0 kB
   ...
SwapTotal:      15998972 kB
SwapFree:       15998972 kB
   ...
```

Prior to running the experiments, I set artificially high system limits on my stack size so
as to prevent the program from failing prematurely in the case of a deep
recursion and force any bottleneck into CPU or memory capacity instead:

```shell
$ ulimit -s 120000 # (kilobytes)
```

and from within the `Python` script:

```python
sys.setrecursionlimit(100000)
```

The flowchart in [@fig:flowchart] shows the overall logic of the driver script (`driver.py`):

![Logic of the batch script (`driver.py`)](source/figures/flowchart.pdf){#fig:flowchart}

Two batches of experiments were run in sequence. The input strings were
chosen from two alphabets: a binary alphabet $\{0,\, 1\}$ and a four-item
alphabet representing a quasi DNA strand $\{A,\, C,\, G,\, T\}$. Input string
length was varied depending on the algorithm to ensure a reasonable runtime
and memory requirements. Strings up to length 20 were used for naive algorithm, up to length 5,000
for the bottom-up dynamic and top-down memoized algorithms, and up to length
40,000 for the Hirschberg algorithm. All algorithms were run on the input strings from
the same library randomly assembled for select string lengths
using the `generate_string.py` module.

Each algorithm implements an essentially identical interface, so that they
can all be run from the driver script with minimum variation. The
`tabulate_lcs` function computes the matrix (or vector, as appropriate)
of LCS lengths. The `reconstruct_lcs` function reconstructs an LCS.

The performance is measured separately for the tasks of

1) computing the length of an LCS, and
2) for reconstructing an LCS,

except for the _naive_ algorithm, where the tasks are coupled.

Profiling the algorithms for time and memory usage is done by wrapping the
above two functions in a `Python` decorator -- a higher-order function that
returns the original function, in addition to logging the time/memory resources.
Similarly, to calculate the depth of recursion, I wrap the helper functions
that are invoked recursively
in a decorator that increments the recursion depth on each invocation.
All of the profiling functions are defined in the `profilers.py` module. Here's
a typical memory profiler output that my measurements are based on. Here
I create a list of characters of length $10^6$ with a footprint of
approximately `8 MB`:

```shell
$ python3 profilers.py

Filename: profilers.py

Line #    Mem usage    Increment   Line Contents
================================================
   155     27.0 MiB      0.0 MiB       @time_and_space_profiler()
   156                                 def mem_test():
   157     27.0 MiB      0.0 MiB           a = 'a'
   158     34.7 MiB      7.7 MiB           b = ['a'] * (10**6)
   159     27.1 MiB     -7.6 MiB           del b
   160     27.1 MiB      0.0 MiB           return a

```

For each algorithm, I ran a suite of tests against hand-computed results to ensure
the program performs as expected, as in the following assertion statements for the _top-down memoized
algorithm_:

\lstinputlisting[language=Python, firstline=219, lastline=261, firstnumber=219]{/home/max/classes/16_spring/algorithms/project/pylib/memoized.py}

Also for verification purposes -- for all strings against which the algorithms were tested --
I plot the lengths  of the reconstructed LCS's in [@fig:sanity]. This shows, as expected, two LCS matches
for each input string length -- consistent with two sets of inputs at each
input string length (binary and DNA alphabet sets) -- except where the two
match strings have identical length or are indistinguishable on the plot scale for the shortest of inputs:

![Sanity check: Verify all algorithms compute the same LCS length for a given pair of input strings](source/figures/gorgon1/sanity_check.ps){#fig:sanity}


In addition to the `Python Standard Library`, I've used the `Python` [`matplotlib`](http://matplotlib.org/)
module for plotting and [`memory_profiler`](https://pypi.python.org/pypi/memory_profiler) to
track memory consumption. Both packages are under the BSD license.

