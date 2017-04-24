# Introduction

[@Galil1991] list a few applications of the `union-find` data
structure and associated algorithms. Of particular interest
for the functional implementation are the applications that include
the possibility of backtracking, such as occur in logic programming --
memory management by Prolog interpreters, incremental execution of
logic programs, unification and search heuristics for resolution
(pattern matching) and the like -- where upon failure, the most recent
unification is undone. Thus, in addition to `UNION` and `FIND-SET`
operations that correspond to unification of terms, there's also a 
`DEUNION(i)` operations that backtracks and
undoes the last $i$ unions performed to recover from the failure and return
to a promising state in the past. Another application is deducing concrete
types from untyped syntax (type inference) in languages such as `Haskell` 
and `OCaml`.
