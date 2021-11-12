# Simplee::Principia::Haskell

## State Monad
[This](https://github.com/veminovici/principia-hs/blob/master/src/State.hs) is my implementation for the *State monad*, inspired by this [blog](https://williamyaoh.com/posts/2020-07-12-deriving-state-monad.html).
*State* module implements the *Functor*, *Applicative*, and *Monad* for the *State* type. 

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


## About this Code

> You can contact me at veminovici@hotmail.com. Code designed and written in Päädu, on the beautiful island of [**Saaremaa**](https://goo.gl/maps/DmB9ewY2R3sPGFnTA), Estonia.
