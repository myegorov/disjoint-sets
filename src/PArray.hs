{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE GADTs #-}

{- |
Module      : PArray
Description : Persistent Mutable Array
License     : BSD3
Maintainer  : findmaksim@gmail.com

Implement mutable persistent array with O(1) access operations
as described in:
https://www.lri.fr/~filliatr/ftp/publis/puf-wml07.pdf

Persistent array implementation based on:
https://hackage.haskell.org/package/diffarray-0.1.1/docs/src/Data-Array-Diff.html#IOToDiffArray

Load with FlexibleContexts flag:
ghci -XFlexibleContexts


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

-- import Data.Array.Base
-- import Data.Array.IArray
import System.IO.Unsafe
import Data.Array.IO
import Data.IORef

-- persistent mutable array is a mutable pointer to one of:
-- * current array
-- * difference list
type PArr a = IORef (AData a)
data AData a =  Arr (IOArray Int a) |
                Diff Int a (PArr a)

-- create new PArr of length n, intialize all elements to value v
create :: Int -> a -> PArr a
create n v0 = unsafePerformIO $ do
    arr <- Data.Array.IO.newArray (0, n-1) v0
    ref <- newIORef (Arr arr)
    return ref

-- create new PArr from list
fromList :: [a] -> PArr a
fromList xs = unsafePerformIO $ do
    arr <- Data.Array.IO.newListArray (0, length xs - 1) xs
    ref <- newIORef (Arr arr)
    return ref

-- create new PArr of length n from function
initialize :: Int -> (Int -> a) -> PArr a
initialize n f =  let xs = map f [0..n-1]
            in fromList xs

-- TODO: refactor after writing reroot
-- access element of PArr
get :: PArr a -> Int -> IO a
get ref i = do
    arr <- readIORef ref
    case arr of 
        Arr arr         -> Data.Array.IO.readArray arr i
        Diff j val ref' -> if i == j
                           then return val
                           else get ref' i

-- TODO: refactor after writing reroot
-- update PArr
set :: PArr a -> Int -> a -> PArr a
set ref ix val = unsafePerformIO $ do
    arr <- readIORef ref
    case arr of
        Diff _ _ _     -> newIORef (Diff ix val ref)
        a@(Arr arr)    -> do
                            oldVal  <- Data.Array.IO.readArray arr ix
                            Data.Array.IO.writeArray arr ix val
                            ref'    <- newIORef a
                            writeIORef ref (Diff ix oldVal ref')
                            return ref'


