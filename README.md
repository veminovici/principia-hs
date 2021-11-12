# Simplee::Principia::Haskell

## State Monad
[This](https://github.com/veminovici/principia-hs/blob/master/src/State.hs) is my implementation for the *State monad*, inspired by this [blog](https://williamyaoh.com/posts/2020-07-12-deriving-state-monad.html).
*State* module implements the *Functor*, *Applicative*, and *Monad* for the *State* type. Below you can see an example how it can be used: in this case we have a function that also increases the number of calls.

```haskell
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

runWorkflow :: s -> State s a -> (s, a)
runWorkflow z (State wf) = 
    wf z

let xs = [1..5];
let ys = [1..10];

-- Build the workflow (appendReverseWithCount) and then run it
-- starting with the initial state set to 0.
let (count, res) = runWorkflow 0 $ appendReverseWithCount xs ys;
```
