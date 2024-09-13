module MyLib where

import Data.Bifunctor (first)
import Numeric.Natural (Natural)
import Math.NumberTheory.Primes (factorise, Prime, unPrime)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

child :: Natural -> Natural
child = buildChildFrom . factorise

buildChildFrom :: [(Prime Natural, Word)] -> Natural
buildChildFrom = product . fmap component
  where component (p, w) = unPrime p + fromIntegral w

terminals :: [Natural]
terminals = [   1
            ,   4
            ,  20 -- 2-loop with 24
            ,   6 -- 2-loop with 12 and 16
            ,  90
            , 120
            ]

easyCutoff :: Natural
easyCutoff = maximum terminals

stepsToTerminal :: Natural -> Word
stepsToTerminal n | n `elem` terminals = 0
                  | otherwise          = 1 + stepsToTerminal (child n)

stepsBack :: Natural -> Natural -> (Word, Natural)
stepsBack threshold n =
  let next = child n
  in if next <= threshold then (1, next)
                  else first ((+) @Word 1) $ stepsBack threshold next

stepsBackX :: Natural -> Word
stepsBackX n | n <= easyCutoff = 0
             | otherwise       = let (steps, result) = stepsBack n n
                                 in if result < n then steps else error $ "Found loop starting at " ++ show n

stupidSearch :: Natural -> Natural -> IO Word
stupidSearch chunksize i =
  do let (start, end) = (1 + (chunksize * (i - 1)), chunksize * i)
     putStrLn $ "Trying " ++ show start ++ " to " ++ show end
     let largest = maximum [stepsBackX n | n <- [start .. end]]
     putStrLn $ "Finished up to " ++ show end ++ ", longest path explored was " ++ show largest
     return largest

