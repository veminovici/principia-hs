{-# LANGUAGE FlexibleContexts #-}
-- | Resource: https://sodocumentation.net/haskell/topic/7752/monad-transformers

module MonadT where

import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict

-- Assume we want to carry out the following computation using the counter:
-- set the counter to 0
-- set the increment constant to 3
-- increment the counter 3 times
-- set the increment constant to 5
-- increment the counter 2 times

--
-- Step 0: No-monad
--
newtype Counter = MkCounter { cValue :: Int } deriving (Show)

-- | Increment the counter by n units
inc :: Counter -> Int -> Counter
inc (MkCounter c) n = MkCounter (c + n)

-- | The computation we want to run.
mComputation :: Counter
mComputation = inc (inc (inc (inc (inc (MkCounter 0) 3) 3) 3) 5) 5

-- | Run and show the computation.
runComputation :: IO ()
runComputation = putStrLn $ show mComputation

--
-- Step 1: State monad
--

-- | CounterS is a monad, the internal state is a Counter.
type CounterS = State Counter

-- | Increment counter by n units
incS :: Int -> CounterS ()
incS n = modify (`inc` n)

mComputationS :: CounterS ()
mComputationS = do
    incS 3
    incS 3
    incS 3
    incS 5
    incS 5

runComputationS :: IO ()
runComputationS = putStrLn $ show . snd $ runState mComputationS (MkCounter 0)

--
-- Step 2: ReaderT on top of State monad. 
--

-- The reader monad provides a convenient way to pass an environment around. 
-- This monad is used in functional programming to perform what in the OO world is known as dependency injection.

-- newtype ReaderT r m a :: * -> (* -> *) -> * -> *
-- The Int is the value that is read from the configuration, 
-- CounterS is the base monad, in our case a State monad.
-- We define an incR function that takes the increment constant from the environment (using ask), 
-- and to define our increment function in terms of our CounterS monad we make use of the lift function (which belongs to the monad transformer class).
type CounterRS = ReaderT Int CounterS

incR :: CounterRS ()
incR = ask >>= lift . incS

-- | The computation we want to run, using reader and state monads.
mComputationRS :: CounterRS ()
mComputationRS = do
    -- Set the local env to return 3
    local (const 3) $ do
        incR
        incR
        incR
        -- Set the local env to return 5
        local (const 5) $ do
            incR
            incR

runComputationRS :: IO ()
runComputationRS = putStrLn $ show . snd $ runState compWithEnv (MkCounter 0)
    where compWithEnv = runReaderT mComputationRS 15

-- Now assume that we want to add logging to our computation, so that we can see the evolution of our counter in time.
-- We also have a monad to perform this task, the writer monad.
-- newtype WriterT w m a :: * -> (* -> *) -> * -> *
-- Here w represents the type of the output to accumulate (which has to be a monoid, which allow us to accumulate this value), m is the inner monad, and a the type of the computation.

type CounterWRS = WriterT [Int] CounterRS

incW :: CounterWRS ()
incW = lift incR >> get >>= tell . (:[]) .cValue

mComputationWRS :: CounterWRS ()
mComputationWRS = do
    -- set the local env to return 3
    local (const 3) $ do
        incW
        incW
        incW
        -- set the local env to return 5
        local (const 5) $ do
            incW
            incW

runComputationWRS :: IO ()
runComputationWRS = putStrLn $ show $ runState compWithEnv (MkCounter 0)
    where compWithEnv = runReaderT compWithLogs 15
          compWithLogs = (runWriterT mComputationWRS)


-- * Doing everything in one go.
-- Credit goes to Bart Frenk.
inc' :: (MonadReader Int m, MonadState Counter m, MonadWriter [Int] m) => m ()
inc' = ask >>= modify . flip inc >> get >>= tell . (:[]) . cValue

mComputation' :: (MonadReader Int m, MonadState Counter m, MonadWriter [Int] m) => m ()
mComputation' = do
  local (const 3) $ do
    inc'
    inc'
    inc'
    local (const 5) $ do
      inc'
      inc'

runComputation' :: IO ()
runComputation' = putStrLn $ show $ runState compWithEnv (MkCounter 0)
  where compWithEnv = runReaderT compWithLogs 15
        compWithLogs = runWriterT mComputation'
