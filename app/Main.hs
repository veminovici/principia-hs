module Main where

import State
import System.IO (putStrLn)
import Data.List (reverse)
import Text.Printf (printf)

-- import Control.Applicative(Alternative)

import Control.Monad(guard, msum)
import Control.Monad.Trans ( MonadTrans(..) )

import MaybeT

--
-- Functions for the State example
--

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

--
-- Functions for the MaybeT example
--

isValid :: String -> Bool
isValid s = length s >= 3

getPassphrase :: MaybeT IO String
getPassphrase = do 
    s <- lift getLine
    guard (isValid s) -- Alternative provides guard.
    return s

askPassphrase :: MaybeT IO ()
askPassphrase = do 
    lift $ putStrLn "Insert your new passphrase (3 chars at least):"
    value <- getPassphrase
    lift $ putStrLn "Storing in database..."

askPassphraseWithRepeat :: MaybeT IO ()
askPassphraseWithRepeat = do 
    lift $ putStrLn "Insert your new passphrase (loop):"
    value <- msum $ repeat getPassphrase
    lift $ putStrLn "Storing in database..."

--
-- Main function
--

main :: IO ()
main = do
    putStrLn "";
    putStrLn "Principia Haskell";

    -- An xample for my State monad
    putStrLn "";
    putStrLn "State monad";

    let xs = [1..5];
    let ys = [1..10];

    -- Build the workflow (appendReverseWithCount) and then run it
    -- starting with the initial state set to 0.
    let (count, res) = let s = appendReverseWithCount xs ys in runState s 0;

    putStrLn $ "ended - count=" ++ show count ++ ", res=" ++ show res;

    putStrLn "";
    putStrLn "MaybeT transformer";

    -- runMaybeT askPassphrase
    runMaybeT askPassphraseWithRepeat

    putStrLn $ "ended"

