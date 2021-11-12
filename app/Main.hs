module Main where

import State
import System.IO (putStrLn)
import Data.List (reverse)
import Text.Printf (printf)


reverseWithCount :: [a] -> State Int [a]
reverseWithCount xs = do
    modify (+1)
    pure (reverse xs)

appendReverseWithCount :: [a] -> [a] -> State Int [a]
appendReverseWithCount xs ys = do
    rxs <- reverseWithCount xs
    rys <- reverseWithCount ys
    modify (+1)
    pure (rxs ++ rys)

main :: IO ()
main = do
    putStrLn "Principia Haskell";

    -- Here is an example for the State monad
    putStrLn "State monad";
    let xs = [1..5];
    let ys = [1..10];
    let (State sxy) = appendReverseWithCount xs ys;
    let (count, res) = sxy 0;
    putStrLn $ "ended - count=" ++ show count ++ ", res=" ++ show res

