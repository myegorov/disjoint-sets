{-# LANGUAGE FlexibleInstances #-}

{- |

Module      : PArray
Description : Persistent Mutable Array
License     : GNU GPLv3
Author      : findmaksim@gmail.com

Implements mutable persistent array (so called "trailer array"):
  + store full array for each leaf node;
  + and a history of updates at each node
=> O(1) access to the most recent version of an array
=> if reverse update pointers on first access to a previous version,
    then amortized O(1) access

For details, see:
S. Conchon and J.-C. Filliâtre. A persistent union-find data structure. In:
Proceedings of the 2007 Workshop on ML, pages 37–46. ACM, 2007.
https://www.lri.fr/~filliatr/ftp/publis/puf-wml07.pdf

-}

module PArray 
( PArr(..)
, AData(..)
, fromList
, len
, create
, initialize
, get
, set ) where

import System.IO.Unsafe
import Data.Array.IO (IOArray, getElems, getBounds,
                        newArray, newListArray,
                        readArray, writeArray)
import Data.IORef

-- persistent mutable array data type
type PArr a = IORef (AData a) -- PArr is a mutable pointer to one of...
data AData a =  Arr (IOArray Int a) | -- current array, or
                Diff Int a (PArr a)   -- difference list

instance Show a =>  Show (PArr a) where
    show ref = show $ toList ref

-- utility function used in show
toList :: PArr a -> [a]
toList ref = 
    let xs = unsafePerformIO $ do
                    reroot ref
                    r <- readIORef ref
                    case r of
                        Arr arr -> getElems arr
    in xs

-- get length of current array
len :: IO (PArr a) -> IO Int
len ref = do
    ref1 <- ref
    reroot ref1
    r    <- readIORef ref1
    case r of
        Arr arr -> do
            (i, j) <- getBounds arr
            return (j - i + 1)
        Diff _ _ _  -> error "len encountered Diff, puzzled..."

-- create new PArr of length n, intialize all elements to value v
create :: Int -> a -> IO (PArr a)
create n v0 = do
    arr <- newArray (0, n-1) v0
    ref <- newIORef (Arr arr)
    return ref

-- create new PArr from list
fromList :: [a] -> IO (PArr a)
fromList xs = do
    arr <- newListArray (0, length xs - 1) xs
    ref <- newIORef (Arr arr)
    return ref

-- create new PArr of length n from function
initialize :: Int -> (Int -> a) -> IO (PArr a)
initialize n f =  let xs = map f [0..n-1]
            in fromList xs


-- modify pointers so as to be able to access array immediately
reroot :: PArr a -> IO ()
reroot ref = do
    r    <- readIORef ref
    case r of
        Arr _            -> return ()
        Diff ix val ref' -> do
            reroot ref'
            r' <- readIORef ref'
            case r' of
                a@(Arr arr) -> do
                    oldVal <- readArray arr ix
                    writeArray arr ix val
                    writeIORef ref a
                    writeIORef ref' (Diff ix oldVal ref)
                    return ()
                Diff _ _ _  -> error "reroot encountered Diff, puzzled..."



-- access element of PArr
get :: IO (PArr a) -> Int -> IO a
get ref i = do
    ref1 <- ref
    r    <- readIORef ref1
    case r of 
        Arr arr         -> readArray arr i
        Diff _ _ _      -> do
            reroot ref1
            r' <- readIORef ref1
            case r' of
                Arr arr     -> readArray arr i
                Diff _ _ _  -> error "get encountered Diff, puzzled..."


-- update operation returns a new persistent array 
set :: (Eq a) => IO (PArr a) -> Int -> a -> IO (PArr a)
set ref ix val = do
    ref1 <- ref
    reroot ref1
    r <- readIORef ref1
    case r of
        a@(Arr arr)    -> do
                            oldVal  <- readArray arr ix
                            -- no need to create new indirection if ix is already mapped to val
                            if oldVal == val
                            then return ref1
                            else do
                                writeArray arr ix val
                                ref'    <- newIORef a
                                writeIORef ref1 (Diff ix oldVal ref')
                                return ref'
        Diff _ _ _     -> error "set encountered Diff, puzzled..."


-- -- usage examples
-- ghci> a0 = create 7 0
-- ghci> a1 = set a0 1 7
-- ghci> a2 = set a1 2 8
-- ghci> a3 = set a1 2 9
-- ghci> a0
-- [0,0,0,0,0,0,0]
-- ghci> a1
-- [0,7,0,0,0,0,0]
-- ghci> a2
-- [0,7,8,0,0,0,0]
-- ghci> a3
-- [0,7,9,0,0,0,0]
