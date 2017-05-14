{-# LANGUAGE FlexibleInstances, GADTs #-}

{- |

Module      : PUnionFind
Description : Persistent Union-Find
License     : GNU GPLv3
Maintainer  : findmaksim@gmail.com

Implement Tarjan's union-find algorithm using persistent arrays,
as described in:
https://www.lri.fr/~filliatr/ftp/publis/puf-wml07.pdf

-}

module PUnionFind where

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
createUF :: Int -> UF Int
createUF size = let
                    r = create size 0
                    p = initialize size id
                in UF {rank=r, parent=p}


-- a helper function to perform path compression
-- returns mutated parent array and representative
_find :: PArr Int -> Int -> (PArr Int, Int)
_find par ix = let ix' = get par ix
               in
                    if ix' == ix
                    then (par, ix)
                    else
                        let (par1, root) = _find par ix'
                            par2         = set par1 ix root
                        in (par2, root)

-- find with path compression
find :: UF Int -> Int -> (UF Int, Int)
find uf elem = let (par, representative) = _find (parent uf) elem
               in (uf { parent=par }, representative)


-- union by rank
union :: UF Int -> Int -> Int -> UF Int
union uf x y = 
    let
        (uf1, reprX) = find uf x
        (uf2, reprY) = find uf1 y
    in 
        if reprX == reprY
        then uf2
        else let
                rankX = get (rank uf2) reprX
                rankY = get (rank uf2) reprY
                in
                if rankX > rankY
                then uf2 { parent=(set (parent uf2) reprY reprX) }
                else if rankY > rankX
                then uf2 { parent=(set (parent uf2) reprX reprY) }
                else uf2 { rank=(set (rank uf2) reprX (rankX+1)), 
                            parent=(set (parent uf2) reprY reprX) }
