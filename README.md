persistent union-find datastructure
===

Tarjan's union-find algorithm implemented using persistent arrays.

For details, see:
[S. Conchon and J.-C. Filliâtre. _A persistent union-find data structure_. In:
_Proceedings of the 2007 Workshop on ML_, pp. 37–46. ACM, 2007.](https://www.lri.fr/~filliatr/ftp/publis/puf-wml07.pdf)

## examples of usage

```haskell
ghci> :l PArray
ghci> a0 = create 7 0
ghci> a1 = set a0 1 7
ghci> a2 = set a1 2 8
ghci> a3 = set a1 2 9
ghci> a0
[0,0,0,0,0,0,0]
ghci> a1
[0,7,0,0,0,0,0]
ghci> a2
[0,7,8,0,0,0,0]
ghci> a3
[0,7,9,0,0,0,0]
```


```haskell
ghci> :l PUnionFind
ghci> t = createUF 10
ghci> t
<UF rank=[0,0,0,0,0,0,0,0,0,0] parent=[0,1,2,3,4,5,6,7,8,9]>
ghci> find t 0
(<UF rank=[0,0,0,0,0,0,0,0,0,0] parent=[0,1,2,3,4,5,6,7,8,9]>,0)
ghci> find t 1
(<UF rank=[0,0,0,0,0,0,0,0,0,0] parent=[0,1,2,3,4,5,6,7,8,9]>,1)
ghci> t1 = union t 0 1
ghci> t1
<UF rank=[1,0,0,0,0,0,0,0,0,0] parent=[0,0,2,3,4,5,6,7,8,9]>
ghci> find t1 0
(<UF rank=[1,0,0,0,0,0,0,0,0,0] parent=[0,0,2,3,4,5,6,7,8,9]>,0)
ghci> find t1 1
(<UF rank=[1,0,0,0,0,0,0,0,0,0] parent=[0,0,2,3,4,5,6,7,8,9]>,0)
ghci> t2 = union t1 2 3
ghci> t2
<UF rank=[1,0,1,0,0,0,0,0,0,0] parent=[0,0,2,2,4,5,6,7,8,9]>
ghci> t3 = union t2 0 3
ghci> t3
<UF rank=[2,0,1,0,0,0,0,0,0,0] parent=[0,0,0,2,4,5,6,7,8,9]>
ghci> (t4, _) <- find t3 3
ghci> t4
<UF rank=[2,0,1,0,0,0,0,0,0,0] parent=[0,0,0,0,4,5,6,7,8,9]>
ghci> t5 = union (return t4) 4 4
ghci> t5
<UF rank=[2,0,1,0,0,0,0,0,0,0] parent=[0,0,0,0,4,5,6,7,8,9]>
ghci> find (return t4) 4
(<UF rank=[2,0,1,0,0,0,0,0,0,0] parent=[0,0,0,0,4,5,6,7,8,9]>,4)
```
