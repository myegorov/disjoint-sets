{-# LANGUAGE GADTs #-}

{- |
Module      : PArray
Description : Persistent Mutable Array
License     : BSD3
Maintainer  : findmaksim@gmail.com

Implement mutable persistent array with O(1) access operations
as described in:
https://www.lri.fr/~filliatr/ftp/publis/puf-wml07.pdf

-}

-- TODO: decide on what will be exported, including constructors
module PArray where
    -- ( PArr
    -- , create
    -- , initialize
    -- , fromList
    -- , len) where
               -- , get
               -- , set
               -- , toList) where

import Data.Array.Base
import Data.Array.IArray
import Data.Array.IO

-- persistent mutable array is a mutable pointer to one of:
-- * current array
-- * difference list
-- newtype PArr a i e =   PA {ref :: Data a i e}
newtype PArr a i e =   PA (Data a i e)
data Data a i e =   DArr (a i e)
                    | Diff (PArr a i e) (Int, e)
type PA = PArr IOArray -- alias


-- create new PArr of length n, intialize all elements to value v
create :: (MArray a e IO, Num i, Ix i) => i -> e -> IO (PArr a i e)
create n v = do
    ar <- newArray (0, n-1) v
    return (PA (DArr ar))

https://hackage.haskell.org/package/diffarray-0.1.1/docs/src/Data-Array-Diff.html#IOToDiffArray
