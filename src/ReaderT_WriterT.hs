-- | Resource: https://gist.github.com/Decoherence/39a4c34685d86a334b63
module ReaderT_WriterT where

import Control.Monad.Reader
import Control.Monad.Writer
import GHC.IO.Handle.Text (hPutStrLn)

data Person = Person { name :: String } deriving (Show)

alice = Person "Alice"
bob = Person "Bob"
carmen = Person "Carmen"

people = [alice, bob, carmen]

-- Get person from environment, log the person, and print name
-- newtype ReaderT r m a :: * -> (* -> *) -> * -> *
-- writerT is the base monad which appends to a string and returns an IO action
-- readerT is the wrapper monad, it reads a Person from environment.
process :: ReaderT Person (WriterT String IO) ()
process = do
    tell "Looking up a person. "
    Person p <- ask
    tell $ "Found person: " ++ p ++ ". "
    liftIO $ putStrLn p

-- WriterT is the base monad which appends to a string and returns an IO action
-- ReaderT is the wrapper monad, it reads a Person from environment.
process' :: ReaderT Person (WriterT String IO) String
process' = do
    tell "Loking up a person "
    Person p <- ask
    tell $ "Found person: " ++ p ++". "
    return p


main :: IO ()
main = do
    -- Print name from monad transformer
    result1 <- runWriterT (runReaderT process alice) -- :: ((), String)
    putStrLn $ snd result1

    -- Extract the name from monad transfomer, then print it
    result2 <- runWriterT (runReaderT process' alice) -- :: (String, String)
    putStrLn $ fst result2
    putStrLn $ snd result2

    -- Now do the same thing for a list of people using mapM
    result3 <- runWriterT (mapM (runReaderT process') people) -- :: ([String], String)

    let people = fst result3
        log    = snd result3

    putStrLn "\n\nReaderT values:\n"
    mapM_ putStrLn people
    
    putStrLn "\nWriterT log:\n"
    putStrLn $ log
    
    return ()
