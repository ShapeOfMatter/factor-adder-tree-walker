module Main where

import Control.Monad(forM_, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Numeric.Natural (Natural)

import MyLib

granularity :: Natural
granularity = 1000 * 1000

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  record <- newIORef (0, 0)

  forM_ [1..] $ chunk record

chunk :: IORef (Word, Natural) -> Natural -> IO ()
chunk ref i = do (record, rindex) <- readIORef ref
                 putStrLn $ "Path to beat was at index " ++ show rindex ++ ", length " ++ show record
                 pathLen <- stupidSearch granularity i
                 when (record < pathLen) $ writeIORef ref (pathLen, i)

