{-# LANGUAGE GADTs #-}

{- |
Module      : PMArray
Description : Persistent Mutable Array
License     : BSD3
Maintainer  : findmaksim@gmail.com

Implement mutable persistent array with O(1) access operations
as described in:
https://www.lri.fr/~filliatr/ftp/publis/puf-wml07.pdf

-}

-- TODO: decide on what will be exported, including constructors
module PMArray
    ( PMArr
    , create
    , initialize
    , fromList
    , len) where
               -- , get
               -- , set
               -- , toList
               -- , fromList ) where

import Control.Monad (liftM)
import Control.Monad.ST
import Data.Array.ST
import Data.STRef

-- persistent mutable array is a mutable pointer to one of:
-- * current array
-- * difference list
-- data PMArr s a =   PMArr (STRef s (Data s a))
data PMArr s a =   PMArr (Data s a)
data Data s a  where
    Data :: (Ix i, Integral i) => 
        (STArray s i a) -> Data s a  -- Data (STArray s Int a)
    Diff :: (Ix i, Integral i) => 
        i -> a -> (PMArr s a) -> Data s a  -- Diff Int a (PMArr s a)


instance (Show a) => Show (PMArr s a) where
    show (PMArr d) = show d
-- TODO: update showsData
instance (Show a) => Show (Data s a) where
    show = showsData
showsData :: (Show a) => Data s a -> String
showsData (Data arr) = "my array"
showsData (Diff ix val ref) = "my array"


-- create new PMArr of length n, intialize all elements to value v
-- create :: (Num i, Ix i) => i -> a -> ST s (STArray s i a)
-- create n v = newArray (0,n-1) v
-- create :: (Num i, Ix i) => i -> a -> ST s (Data s a)
create :: (Num i, Integral i, Ix i) => i -> a -> ST s (PMArr s a)
create n v = PMArr `liftM` (Data `liftM` (newArray (0,n-1) v))

-- create new PMArr from a list
-- fromList :: (Num i, Ix i) => [a] -> ST s (STArray s i a)
-- fromList :: (Num i, Ix i) => [a] -> ST s (Data s a)
fromList :: (Num i, Integral i) => [a] -> ST s (PMArr s a)
fromList xs =  PMArr `liftM` (Data `liftM` (newListArray
                                (fromIntegral 0, fromIntegral ((length xs)-1)) 
                                xs))

-- create new PMArr of length n, initialize all elements using function f
-- applied to indices of the new array
initialize :: (Num i, Enum i) => i -> (i -> a) -> ST s (PMArr s a)
initialize n f = let xs = map f [0..n-1] in fromList xs

-- return length of PMArr
len :: (Integral i, Enum i, Ix i) => PMArr s a -> ST s Integer
len (PMArr (Data arr)) = fromIntegral `liftM` ((succ . snd) `liftM` getBounds arr)
