{-# LANGUAGE FlexibleInstances, GADTs #-}

{- |

Module      : PUnionFind
Description : Persistent Union-Find
License     : GNU GPLv3
Author      : findmaksim@gmail.com

Implements Tarjan's union-find algorithm using persistent arrays.

For details, see:
S. Conchon and J.-C. Filliâtre. A persistent union-find data structure. In:
Proceedings of the 2007 Workshop on ML, pages 37–46. ACM, 2007.
https://www.lri.fr/~filliatr/ftp/publis/puf-wml07.pdf

-}

module PUnionFind
( UF(..)
, createUF
, union
, find ) where

import PArray

data UF a where 
    UF :: Num a => { 
        rank :: PArr a,
        parent :: PArr a
    } -> UF a

instance Show a => Show (UF a) where
    show UF {rank=r, parent=p} = 
        "<UF rank=" ++ show r ++ " parent=" ++ show p ++ ">"

-- create new union-find structure
createUF :: Int -> IO (UF Int)
createUF size = do
                    r <- create size 0
                    p <- initialize size id
                    return (UF {rank=r, parent=p})


-- a helper function to perform path compression
-- returns mutated parent array and representative
_find :: IO (PArr Int) -> Int -> IO (PArr Int, Int)
_find par ix = do 
                    par' <- par
                    ix' <- get par ix
                    if ix' == ix
                    then return (par', ix)
                    else do
                        (par1, root) <- _find par ix'
                        par2         <- set (return par1) ix root
                        return (par2, root)

-- find with path compression
find :: IO (UF Int) -> Int -> IO (UF Int, Int)
find uf elem = do
                    uf1                   <- uf
                    (par, representative) <- _find (return (parent uf1)) elem
                    return (uf1 { parent=par }, representative)


-- union by rank
union :: IO (UF Int) -> Int -> Int -> IO (UF Int)
union uf x y = do
        (uf1, reprX) <- find uf x
        (uf2, reprY) <- find (return uf1) y
        if reprX == reprY
        then return uf2
        else do
                rankX <- get (return (rank uf2)) reprX
                rankY <- get (return (rank uf2)) reprY
                if rankX > rankY
                then do
                    par1  <- set (return (parent uf2)) reprY reprX
                    return (uf2 { parent=par1 })
                else if rankY > rankX
                then do
                    par1  <- set (return (parent uf2)) reprX reprY
                    return (uf2 { parent=par1 })
                else do
                    rank1 <- set (return (rank uf2)) reprX (rankX+1)
                    par1  <- set (return (parent uf2)) reprY reprX
                    return (uf2 { rank=rank1, parent=par1 })

-- -- usage examples
-- ghci> t = createUF 10
-- ghci> t
-- <UF rank=[0,0,0,0,0,0,0,0,0,0] parent=[0,1,2,3,4,5,6,7,8,9]>
-- ghci> find t 0
-- (<UF rank=[0,0,0,0,0,0,0,0,0,0] parent=[0,1,2,3,4,5,6,7,8,9]>,0)
-- ghci> find t 1
-- (<UF rank=[0,0,0,0,0,0,0,0,0,0] parent=[0,1,2,3,4,5,6,7,8,9]>,1)
-- ghci> t1 = union t 0 1
-- ghci> t1
-- <UF rank=[1,0,0,0,0,0,0,0,0,0] parent=[0,0,2,3,4,5,6,7,8,9]>
-- ghci> find t1 0
-- (<UF rank=[1,0,0,0,0,0,0,0,0,0] parent=[0,0,2,3,4,5,6,7,8,9]>,0)
-- ghci> find t1 1
-- (<UF rank=[1,0,0,0,0,0,0,0,0,0] parent=[0,0,2,3,4,5,6,7,8,9]>,0)
-- ghci> t2 = union t1 2 3
-- ghci> t2
-- <UF rank=[1,0,1,0,0,0,0,0,0,0] parent=[0,0,2,2,4,5,6,7,8,9]>
-- ghci> t3 = union t2 0 3
-- ghci> t3
-- <UF rank=[2,0,1,0,0,0,0,0,0,0] parent=[0,0,0,2,4,5,6,7,8,9]>
-- ghci> (t4, _) <- find t3 3
-- ghci> t4
-- <UF rank=[2,0,1,0,0,0,0,0,0,0] parent=[0,0,0,0,4,5,6,7,8,9]>
-- ghci> t5 = union (return t4) 4 4
-- ghci> t5
-- <UF rank=[2,0,1,0,0,0,0,0,0,0] parent=[0,0,0,0,4,5,6,7,8,9]>
-- ghci> find (return t4) 4
-- (<UF rank=[2,0,1,0,0,0,0,0,0,0] parent=[0,0,0,0,4,5,6,7,8,9]>,4)
