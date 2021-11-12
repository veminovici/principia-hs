# Simplee::Principia::Haskell

## Build & Run
```
stack build
stack run
```

## State Monad
[This](https://github.com/veminovici/principia-hs/blob/master/src/State.hs) is my implementation for the *State monad*, inspired by this [blog](https://williamyaoh.com/posts/2020-07-12-deriving-state-monad.html).
*State* module implements the *Functor*, *Applicative*, and *Monad* for the *State* type as well as some utility functions:

```haskell
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
let (count, res) = runWorkflow 0 $ appendReverseWithCount xs ys;
```

## Reader Monad
[This](https://github.com/veminovici/principia-hs/blob/master/src/Reader.hs) is my implementation for the *Reader monad*. It is inspired by this [blog](https://williamyaoh.com/posts/2020-07-19-deriving-reader-monad.html).
*Reader* module implements the *Functor*, *Applicative*, and *Monad* for the *Reader* type as well as some utility functions:

```haskell
-- | Creates a Reader workflow which returns the configuration
ask :: Reader c c

-- | Creates a Reader workflow from a function.
asks :: (c -> a) -> Reader c a

-- | Helps running a workflow using a different config (e.g. local configuration)
-- and then return to the original configuration. c' is the local configuration
-- and c is the original configuration.
local :: (c -> c') -> Reader c' a -> Reader c a
```

## About this Code

> You can contact me at veminovici@hotmail.com. Code designed and written in Päädu, on the beautiful island of [**Saaremaa**](https://goo.gl/maps/DmB9ewY2R3sPGFnTA), Estonia.
