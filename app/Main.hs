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

-- | Runs a State workflow starting from an initial state.
runWorkflow :: s -> State s a -> (s, a)
runWorkflow z (State wf) = 
    wf z

main :: IO ()
main = do
    putStrLn "Principia Haskell";

    -- An xample for my State monad
    putStrLn "State monad";

    let xs = [1..5];
    let ys = [1..10];

    -- Build the workflow (appendReverseWithCount) and then run it
    -- starting with the initial state set to 0.
    let (count, res) = runWorkflow 0 $ appendReverseWithCount xs ys;

    putStrLn $ "ended - count=" ++ show count ++ ", res=" ++ show res

