# Introduction

I implement and investigate the performance of four algorithms that
each calculate the length of and reconstruct a longest common
subsequences shared by two strings. The algorithms are -- in the order of
increasing sophistication -- the naive recursive, top-down memoized recursive,
bottom-up dynamic iterative, and Hirschberg's quadratic time linear space
recursive algorithms.

The implementation of all algorithms except Hirschberg's quadratic-time linear-space algorithm is based on [@Cormen2009].
For Hirschberg's Algorithm B and Algorithm C, see [@Hirschberg1975].

This is an empirical investigation of the actual runtime performance. The
algorithms where implemented using the [`Python`](https://www.python.org/)
programming language. `Python` is a high-level interpreted language. The
reason that I chose `Python` is that it offers a near pseudocode-level
clarity of the implementation. The drawback are the comparatively
long execution times, as will become clear from the data below.
For this exercise where we merely compare the algorithms
among themselves -- without worrying about putting them in production --
`Python` proved to be an adequate choice, especially from the standpoint of rapidly
coming up with a prototype implementation.

The algorithms were run remotely on a CS department lab machine (`gorgon.cs.rit.edu`)
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
as to prevent the program from failing prematurely from a too deep
recursion and force any bottleneck in CPU or memory capacity instead:

```shell
$ ulimit -s 120000 # (kilobytes)
```

and from `Python`:

```python
sys.setrecursionlimit(100000)
```

The flowchart below shows the overall logic of the driver script (`driver.py`):
![Logic of the batch script](source/figures/flowchart.pdf)

Two batches of experiments were run in total. The input strings were
chosen from two alphabets: a binary alphabet $\{0,\, 1\}$ and a four-item
alphabet representing a DNA strand $\{A,\, C,\, G,\, T\}$. Input string
length was varied depending on the algorithm to ensure a reasonable runtime
and memory requirements. Strings up to length 20 were used for naive algorithm, up to length 5,000
for the bottom-up dynamic and top-down memoized algorithms, and up to length
40,000 for the Hirschberg algorithm. All algorithms are run wiht input strings from
the same library randomly assembled for a given string length
using the `generate-string.py` module.

Each algorithm implements an essentially identical interface, so that they
can all be run from the driver script with minimum custom code. The
`tabulate_lcs()` function computes the matrix (or vector, as appropriate)
of LCS lengths. The `reconstruct_lcs()` function reconstructs an LCS.

The performance is measured separately for the tasks of computing the length of an LCS,
and for reconstructing an LCS -- except for the _naive_ algorithm, where the
tasks cannot be separated.

Profiling the algorithms for time and memory consumption is done by wrapping the
two functions in a `Python` decorator a higher-order function that
returns the original function, in addition to logging the time/memory resources.
Similarly, to calculate the depth of recursion, I wrap the helper functions of
the above in a decorator that increments the recursion depth on each invocation.
All of the profiling functions are defined in the `profilers.py` module.

For each algorithm, I ran a suite of tests against hand-computed results to ensure
the program performs as expected, as in the following assertion statements for the _top-down memoized
algorithm_:

\lstinputlisting[language=Python, firstline=219, lastline=261, firstnumber=219]{/home/max/classes/16_spring/algorithms/project/pylib/memoized.py}

Also, for the library of strings against which all algorithms were tested,
I plot the length  of LCS below. This shows, as expected, two LCS matches
for each input string length -- consistent with two sets of inputs at each
input string length (binary and DNA alphabet sets):

![Sanity check: Verify all algorithms compute the same LCS length for a given pair of input strings](source/figures/gorgon1/sanity_check.ps)

In addition to the `Python Standard Library`, I've used the `Python` [`matplotlib`](http://matplotlib.org/)
module for plotting and [`memory_profiler`](https://pypi.python.org/pypi/memory_profiler) to
track memory usage. Both packages are under the BSD license.

