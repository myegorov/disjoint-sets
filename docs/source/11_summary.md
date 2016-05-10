# Summary of results {#sec:summary}

Two sets of experiments have been performed. They show the same tendencies,
but the actual execution time and memory usage occasionally differs, which
demonstrates the vagaries of attaching too much precision beyond the
approximate asymptotic estimates. In this section, I compare experimental runs side by side.

## Set 1

Note that I distinguish the tasks of sizing and reconstructing the LCS for
all algorithms except the _naive algorithm_.
From [@fig:61] it can be seen that the execution time of
the
three algorithms for sizing LCS (excluding _naive_) is quadratic in the length of input string. What is truly
remarkable is how much more efficient Hirschberg's _Algorithm B_ is compared
even to its very close cousin _dynamic bottom-up algorithm_. Essentially,
the only difference between the algorithms is that the _dynamic bottom-up algorithm_
keeps an in-memory matrix of lengths that is the size of Hirschberg's vector in-memory storage squared.

![Set 1: Runtime vs input length -- sizing LCS](source/figures/gorgon1/cpu_input_sizing.ps){#fig:61}

[@fig:62] demonstrates vividly the inefficiency of the _naive algorithm_
that takes longer than a Hirschberg's algorithm on an input that is three orders of magnitude
naive's. One can also clearly see the quadratic nature of Hirschberg's
reconstruction scheme (for the CPU time, as opposed to memory usage). Compare
this to linear time reconstruction algorithms (_dynamic_ and _memoized_).

![Set 1: Runtime vs input length -- reconstructing LCS](source/figures/gorgon1/cpu_input_reconstruct.ps){#fig:62}

[@fig:63] illustrates the difference between recursive and iterative algorithms.
For the recursive _memoized algorithm_ (_naive_ not shown, as it performs
reconstruction coupled with sizing the LCS), one can see the quadratic
nature of recursion depth vs. input string length. This will become even
clearer on set 2 plots below. By distinction, _dynamic_ and _Hirschberg_ algorithms
are iterative.

![Set 1: Recursion depth vs input length -- sizing LCS](source/figures/gorgon1/recursion_input_sizing.ps){#fig:63}

Finally, [@fig:64] shows the quadratic relationship between
memory usage and input length for _dynamic_ and _memoized_ algorithms, as
opposed to linear relationship for _Hirschberg_, which barely grows for
its very low footprint.

![Set 1: Memory usage -- sizing LCS](source/figures/gorgon1/mem_usage.ps){#fig:64}


## Set 2

The second run has broadly comparable results. Remarkably, there are sometimes
dramatic differences, which demonstrates the risk of estimating the runtime or memory
consumption with more precision than can be justified. Compare for example
the tables in @sec:results-tables for the _dynamic_ runs 1 and 2 for
input of size 5000.

For better resolution, the plots for set 2 exclude the runs for inputs
of size above 5,000 (see instead set 1 plots for _Hirschberg algorithm_
inputs for sizes $> 5,000$).

We observe from the plots that alphabetic input matching appears to be less efficient than binary.
This is probably due to the fact that the longer length of matched strings
(for the quasi-random algorithm I used in generating input strings) results
in faster "convergence" for binary strings compared to DNA strings. Refer to
[@fig:69] and to fig. 1.2. It would be interesting to compare the efficiency
if the length of match were controlled for.

_Memoized_ scheme
is less efficient than _dynamic_, which is probably due to the overhead from
recursion (vs. iterative implementation of the _dynamic_ algorithm). _Hirschberg's_
implementation (also iterative), trumps _dynamic_ by far in virtue of its
lean operations on vector storage of the LCS lengths (vs. 2D matrix in case of the
_dynamic_ algorithm). It should be mentioned that I used the rather inefficient
storage scheme using $m \times n$ sized lists from Python's Standard Library instead of using arrays from
the outside `numpy` library that are much more compact
and efficient.

![Set 2: Runtime vs input length -- sizing LCS](source/figures/gorgon2/cpu_input_sizing.ps){#fig:65}

With reference to [@fig:66]: Reconstructing an LCS match using the naive algorithm is tremendously
inefficient. The distinction between exponential and polynomial algorithm
is evident in this plot, where maximum-length _naive_
input is 20, evidently due to its wastefull recursive calls.
Note the depth of recursion in [@fig:67] even for such a small input size.

![Set 2: Runtime vs input length -- reconstructing LCS](source/figures/gorgon2/cpu_input_reconstruct.ps){#fig:66}

![Set 2: Recursion depth vs input length -- reconstructing LCS](source/figures/gorgon2/recursion_input_reconstruct.ps){#fig:67}

With reference to [@fig:69]: For recursive algorithms, the quadratic relationship between recursion vs input length
mirrors that between CPU time vs input length. There's a linear relationship
between recursion depth and CPU time for the recursive _memoized algorithm_.

![Set 2: Recursion depth vs input length -- sizing LCS](source/figures/gorgon2/recursion_input_sizing.ps){#fig:68}

It is interesting
to note that it takes about twice as many recursive calls for an alphabetic string
compared to binary string -- for the same algorithm and string length input! Note that the DNA
alphabet is also twice the size of the binary alphabet. Again, I suspect this is
due to the longer match and correspondingly faster convergence, which is
accidental, in the sense that it is not intrinsic to the alphabet representation
in my case but is just a fluke of string generation.

![Set 2: Runtime vs recursion depth -- sizing LCS](source/figures/gorgon2/cpu_recursion_sizing.ps){#fig:69}

With reference to [@fig:610], the memory usage is also quadratic in the length of input
for all algorithms, except _Hirschberg's_, which is linear as expected (barely
noticeable footprint). This is expected for 2D tables. Also, one notes the
difference between the _dynamic_ and _memoized_ memory usage for the __same__
input strings! This is not due to anything intrinsic in the algorithms. One
would expect that the two algorithms would have identical memory usage.
The difference is explained by my implementation: I just happened to use
very sparsely populated arrays (mostly filled by `None` pointers) for the
_memoized_ implementation. Whereas, all entries in the  _dynamic_ arrays are initialized to
`0`. I didn't put much thought into the difference of implementation,
but it obviously led to some dramatic difference in memory usage.

![Set 2: Memory usage -- sizing/reconstructing LCS](source/figures/gorgon2/mem_usage.ps){#fig:610}

<!--
To include a reference, add the citation key shown in the references.bib file.
-->


<!--
### Subsection of the middle bit

This is a subsection of the middle bit. Quisque sit amet tempus arcu, ac suscipit ante. Cras massa elit, pellentesque eget nisl ut, malesuada rutrum risus. Nunc in venenatis mi. Curabitur sit amet suscipit eros, non tincidunt nibh. Phasellus lorem lectus, iaculis non luctus eget, tempus non risus. Suspendisse ut felis mi.
-->

<!--
For italic, add one * on either side of the text
For bold, add two * on either side of the text
For bold and italic, add _** on either side of the text
-->



