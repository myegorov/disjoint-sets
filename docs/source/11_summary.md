# Summary of results {#sec:summary}

In this section, I compare experimental runs side by side.
The following plot excludes _naive algorithm_ results so as to distinguish
sizing LCS from reconstruction. It can be seen that the execution time of
the remaining
three algorithms is polynomial in the length of input string. What is truly
remarkable is how much more efficient Hirschberg's _Algorithm B_ is compared
even to its very close cousin _dynamic bottom-up algorithm_. Essentially,
the only difference between the algorithms is that the _dynamic bottom-up algorithm_
keeps an in-memory matrix of lengths that is the size of Hirschberg's vector in-memory storage squared.

Figure \ref{cpu-vs-len-dynamic} shows a run time profiler
output for a string of size 10,000.

![Runtime vs input length: sizing LCS](source/figures/gorgon1/cpu_input_sizing.ps)

The following plot demonstrates vividly the inefficiency of the _naive algorithm_
that takes longer than a Hirschberg's algorithm on an input that is three orders of magnitude
naive's. One can also clearly see the polynomial nature of Hirschberg's
reconstruction scheme (for the CPU time, as opposed to memory usage).

![Runtime vs input length: reconstructing LCS](source/figures/gorgon1/cpu_input_reconstruct.ps)

For the recursive _memoized algorithm_ (_naive_ not shown, as it performs
reconstruction coupled with sizing the LCS), one can see the polynomial
nature of recursion depth vs. input string length. This will become even
clearer on set 2 plots below. By distinction, _dynamic_ and _Hirschberg_ algorithms
are iterative.

![Recursion depth vs input length: sizing LCS](source/figures/gorgon1/recursion_input_sizing.ps)

Finally, the following plot shows the polynomial relationship between
memory usage and input length for _dynamic_ and _memoized_ algorithms, as
opposed to linear relationship for _Hirschberg_, which barely grows for
its very low footprint.

![Memory usage](source/figures/gorgon1/mem_usage.ps)


I performed a second run, with essentially identical results.
For better resolution, the following plots exclude the runs for inputs
of size above 5,000 (_Hirschberg algorithm_).

All graphs are clearly polynomial in the length of input. Interestingly,
alphabetic input matching is less efficient than binary. _Memoized_ scheme
is less efficient than _dynamic_, which is probably due to the overhead from
recursion (vs. iterative implementation of the _dynamic_ algorithm). _Hirschberg's_
implementation (also iterative), trumps _dynamic_ by far in virtue of its
lean operations on vector storage of the LCS lengths (vs. 2D matrix in case of the
_dynamic_ algorithm). It should be mentioned that I used the rather inefficient
storage scheme using m x n-sized lists from Python's Standard Library instead of using arrays from
the outside `numpy` library that are much more compact
and efficient.

![Runtime vs input length: sizing LCS](source/figures/gorgon2/cpu_input_sizing.ps)

Reconstructing an LCS match using the naive algorithm is tremendously
inefficient. The distinction between exponential and polynomial algorithm
is starkly evident in the following plot, where maximum-length _naive_
input is 20, evidently due to its wastefull recursive calls.
Note the depth of recursion even for such a small input size.

![Runtime vs input length: reconstructing LCS](source/figures/gorgon2/cpu_input_reconstruct.ps)

![Recursion depth vs input length: reconstructing LCS](source/figures/gorgon2/recursion_input_reconstruct.ps)

For recursive algorithms, the polynomial relationship between recursion vs input length
mirrors that between CPU time vs input length. The two are clearly related.

![Recursion depth vs input length: sizing LCS](source/figures/gorgon2/recursion_input_sizing.ps)

This observation is reinforced by the following plot that shows CPU runtime vs recursion depth.
For the recursive _memoized_ algorithm, the relationship is clearly linear. What is interesting
to note is that it takes about twice as many recursive calls for an alphabetic string
compared to binary string -- for the same algorithm and string length input! Note that the DNA
alphabet is also twice the size of the binary alphabet.

![Runtime vs recursion depth: sizing LCS](source/figures/gorgon2/cpu_recursion_sizing.ps)

Finally, the memory usage is also polynomial in the length of input
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

![Memory usage](source/figures/gorgon2/mem_usage.ps)

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



