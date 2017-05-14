{-# LANGUAGE FlexibleInstances #-}

{- |

Module      : PArray
Description : Persistent Mutable Array
License     : GNU GPLv3
Maintainer  : findmaksim@gmail.com

Implement mutable persistent array with O(1) access operations
as described in:
https://www.lri.fr/~filliatr/ftp/publis/puf-wml07.pdf

-}

module PArray where

import System.IO.Unsafe
import Data.Array.IO
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
                    Arr arr -> Data.Array.IO.getElems arr
    in xs

-- get length of current array
len :: PArr a -> Int
len ref = unsafePerformIO $ do
    reroot ref
    r <- readIORef ref
    case r of
        Arr arr -> do
            (i, j) <- Data.Array.IO.getBounds arr
            return (j - i + 1)
        Diff _ _ _  -> error "len encountered Diff, puzzled..."

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


-- modify pointers so as to be able to access array immediately
reroot :: PArr a -> IO ()
reroot ref = do
    r <- readIORef ref
    case r of
        Arr _            -> return ()
        Diff ix val ref' -> do
            reroot ref'
            r' <- readIORef ref'
            case r' of
                a@(Arr arr) -> do
                    oldVal <- Data.Array.IO.readArray arr ix
                    Data.Array.IO.writeArray arr ix val
                    writeIORef ref a
                    writeIORef ref' (Diff ix oldVal ref)
                    return ()
                Diff _ _ _  -> error "reroot encountered Diff, puzzled..."



-- access element of PArr
-- get :: PArr a -> Int -> IO a
get :: PArr a -> Int -> a
get ref i = unsafePerformIO $ do
    r <- readIORef ref
    case r of 
        Arr arr         -> Data.Array.IO.readArray arr i
        Diff _ _ _      -> do
            reroot ref
            r' <- readIORef ref
            case r' of
                Arr arr     -> Data.Array.IO.readArray arr i
                Diff _ _ _  -> error "get encountered Diff, puzzled..."


-- update operation returns a new persistent array 
set :: (Eq a) => PArr a -> Int -> a -> PArr a
set ref ix val = unsafePerformIO $ do
    reroot ref
    r <- readIORef ref
    case r of
        a@(Arr arr)    -> do
                            oldVal  <- Data.Array.IO.readArray arr ix
                            -- no need to create new indirection if ix is already mapped to val
                            if oldVal == val
                            then return ref
                            else do
                                Data.Array.IO.writeArray arr ix val
                                ref'    <- newIORef a
                                writeIORef ref (Diff ix oldVal ref')
                                return ref'
        Diff _ _ _     -> error "set encountered Diff, puzzled..."
