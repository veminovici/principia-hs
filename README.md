!["Haskell](./assets/images/haskell.png "Haskell") 
# Simplee::Principia::Haskell

## Content
- [Build & Run](https://github.com/veminovici/principia-hs#build--run)
- [State Monad](https://github.com/veminovici/principia-hs#)
- [Reader Monad](https://github.com/veminovici/principia-hs#-1)
- [Writer Monad](https://github.com/veminovici/principia-hs#-2)
- [MoaybeT Transformer](https://github.com/veminovici/principia-hs#-3)
- [Lenses, Prisms, Isomorphism, Epimorphism](https://github.com/veminovici/principia-hs#-4)

<br/>

## Build & Run
```
stack build
stack run
```

## ![State monad](./assets/images/state.png "State monad")

[This](https://github.com/veminovici/principia-hs/blob/master/src/State.hs) is my implementation for the *State monad*, inspired by this [blog](https://williamyaoh.com/posts/2020-07-12-deriving-state-monad.html).
*State* module implements the *Functor*, *Applicative*, and *Monad* for the *State* type as well as some utility functions:

```haskell
-- | A state is a function from a state to a tuple which contains the new state and returned value.
newtype State s a = State { runState :: s -> (s, a) }

instance Functor (State s) where ...
instance Applicative (State s) where ...
instance Monad (State s) where ...

-- | State workflow which returns the internal state.
get :: State s s

-- | State workflow which sets the internal state.
put :: s -> State s ()

-- | State workflow which updates the internal state to a value
-- returned by a given function.
modify :: (s -> s) -> State s () 
```

Below you can see an example how it can be used: in this case we have a function that also increases the number of calls.

```haskell
-- | Creates a State workflow which given a list it returns as result 
-- the reverse of the list while incrementing a counter (which is the state).
reverseWithCount :: [a] -> State Int [a]
reverseWithCount xs = do
    modify (+1)
    pure (reverse xs)

-- | Creates another State workflow, using do notation and other states.
-- It takes two lists, reverses them, and combines the results into one list.
appendReverseWithCount :: [a] -> [a] -> State Int [a]
appendReverseWithCount xs ys = do
    rxs <- reverseWithCount xs
    rys <- reverseWithCount ys
    modify (+1)
    pure (rxs ++ rys)

-- | This is a generic function which takes a State workflow and an initial state
-- and executes the workflow.
runWorkflow :: s -> State s a -> (s, a)
runWorkflow z (State wf) = 
    wf z

-- Starting with two lists
let xs, ys = [1..5], [1..10];

-- Build the workflow (appendReverseWithCount) and then run it
-- starting with the initial state set to 0.
let (count, res) = let s = appendReverseWithCount xs ys in runState s 0;
-- count=3 and res=[5,4,3,2,1,10,9,8,7,6,5,4,3,2,1]
```

## ![Reader monad](./assets/images/reader.png "Reader monad")

[This](https://github.com/veminovici/principia-hs/blob/master/src/Reader.hs) is my implementation for the *Reader monad*. It is inspired by this [blog](https://williamyaoh.com/posts/2020-07-19-deriving-reader-monad.html).
*Reader* module implements the *Functor*, *Applicative*, and *Monad* for the *Reader* type as well as some utility functions:

```haskell
-- | Reader is a function that takes a configuration as input and returns a value.
newtype Reader c a = Reader { runReader :: c -> a }

instance Functor (Reader c) where ...
instance Applicative (Reader c) where ...
instance Monad (Reader c) where ...

-- | Creates a Reader workflow which returns the configuration
ask :: Reader c c

-- | Creates a Reader workflow from a function.
asks :: (c -> a) -> Reader c a

-- | Helps running a workflow using a different config (e.g. local configuration)
-- and then return to the original configuration. c' is the local configuration
-- and c is the original configuration.
local :: (c -> c') -> Reader c' a -> Reader c a
```

## ![Writer monad](./assets/images/writer.png "Writer monad")

[This](https://github.com/veminovici/principia-hs/blob/master/src/Writer.hs) is my implementation for the *Writer monad*. It is inspired by this [blog](https://williamyaoh.com/posts/2020-07-26-deriving-writer-monad.html).
*Writer* module implements the *Functor*, *Applicative*, and *Monad* for the *Reader* type as well as some utility functions:

```haskell
-- | Writer is tuple where the first element is an accumulator and the second one is a returned value.
newtype Writer log a = Writer { runWriter :: (log, a) }

instance Functor (Writer log) where ...
instance Monoid log => Applicative (Writer log) where ...
instance Monoid log => Monad (Writer log) where ...

-- | Appends to the log.
appendLog :: Monoid log => log -> Writer log a -> Writer log a

-- | Sets the internal log to a given value
tell :: log -> Writer log ()

-- | Modified the internal log.
censor :: (log -> log) -> Writer log a -> Writer log a

-- | Returns the current log along with the given value.
listen :: Writer log a -> Writer log (a, log)
```

## ![MaybeT trasformer](./assets/images/maybet.png "MaybeT trasformer")

[This](https://github.com/veminovici/principia-hs/blob/master/src/MaybeT.hs) is my implementation for the *Maybet transformer*. It is inspired by this [wiki](https://en.wikibooks.org/wiki/Haskell/Monad_transformers).
*MaybeT* modely implements the *Functor*, *Applicative*, and *Monad* for the *MaybeT* type. Also some convenient classes are also implemented: *Alternative* and *ModanPlus* [(see more)](https://en.m.wikibooks.org/wiki/Haskell/Alternative_and_MonadPlus).

```haskell
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Functor (MaybeT m) where ...
instance Monad m => Applicative (MaybeT m) where ...
instance Monad m => Monad (MaybeT m) where ...

instance Monad m => Alternative (MaybeT m) where ...
instance Monad m => MonadPlus (MaybeT m) where ...
instance MonadTrans MaybeT where ...
```

Below you can see an example how it can be used:

```haskell
import Control.Monad(guard, msum)
import Control.Monad.Trans ( MonadTrans(..) )
import MaybeT

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

main :: IO ()
main = do
    putStrLn "MaybeT transformer";
    runMaybeT askPassphrase
```

## ![Lens](./assets/images/lpie.png "Lens")

[This](https://github.com/veminovici/principia-hs/blob/master/src/Isomorphism.hs) is my implementation for the *Lens*, *Isomorphism*, and *Epimorphism*. 
It is inspired by this [blog](https://xyncro.tech/aether/guides/lenses.html).

```haskell
data Lens a b = Lens (a -> b) (b -> a -> a)
data Isomorphism a b = Iso (a -> b) (b -> a)
data Epimorphism a b = Epi (a -> Maybe b) (b -> a)
```

## ![Zippers](./assets/images/zippers.png "Zippers")


## About this Code

> You can contact me at veminovici@hotmail.com. Code designed and written in Päädu, on the beautiful island of [**Saaremaa**](https://goo.gl/maps/DmB9ewY2R3sPGFnTA), Estonia.
