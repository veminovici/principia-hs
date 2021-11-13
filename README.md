!["Haskell](./assets/images/haskell.png "Haskell") 
# Simplee::Principia::Haskell

## Contents
- [Build & Run](https://github.com/veminovici/principia-hs#build--run)
- [State Monad](https://github.com/veminovici/principia-hs#)
- [Reader Monad](https://github.com/veminovici/principia-hs#-1)
- [Writer Monad](https://github.com/veminovici/principia-hs#-2)
- [MaybeT Transformer](https://github.com/veminovici/principia-hs#-3)
- [Lenses, Prisms, Isomorphism, Epimorphism](https://github.com/veminovici/principia-hs#-4)
- [Zippers](https://github.com/veminovici/principia-hs#-5)

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
-- | Lens is a pair of getter and setter. The a is the container and b is its property.
data Lens a b = Lens (a -> b) (b -> a -> a)

-- | Prism is a pair of getter and setter, where the property is optional
data Prism a b = Prism (a -> Maybe b) (b -> a -> a)

-- | Isomorphism is a pair of conversion functions for two types.
-- The conversion in both ways is always available (eg String <-> [Char])
data Isomorphism a b = Iso (a -> b) (b -> a)

-- | Epimorphism is a pair of coversion functions for two types.
-- The conversion may not work all the type (eg String <-> Int)
data Epimorphism a b = Epi (a -> Maybe b) (b -> a)
```

## ![Zippers](./assets/images/zippers.png "Zippers")
[Zippers](https://github.com/veminovici/principia-hs/blob/master/src/Zippers.hs) are a mechanism to navigate in a tree/list and focus on a specific node/item.
This code is inspired by this [wiki](http://learnyouahaskell.com/zippers).

Let's assume that we have a binary tree, where each node has a left and rght subtree, than we can navigate using a list of directions (turn left, turn right)

```haskell
data BTree a = BEmpty | BNode a (BTree a) (BTree a)

-- | We can take a turn to left of right
data Direction = L | R
-- | Directions is just a list of turns we took
type Directions = [Direction]

-- | A crumb remembers the node value, the turn we take and the subtree not visited
data Crumb a = LeftCrumb a (BTree a) | RightCrumb a (BTree a)
-- | Breadcrumbs is just a collection of crumbs
type Breadcrumbs a = [Crumb a]

-- | Given a node and a accumulated list of crumbs, 
-- we take left returning the left subtree and append a new crumb (value and right subtree)
goLeft :: (BTree a, Breadcrumbs a) -> (BTree a, Breadcrumbs a)
goLeft (BNode n l r, bs) = (l, LeftCrumb n r:bs)

-- | Given a node and an aaccumulated list of crumbs, 
-- we take right returning the right subtree and append a new crumb (value and lft subtree)
goRight :: (BTree a, Breadcrumbs a) -> (BTree a, Breadcrumbs a)
goRight (BNode n l r, bs) = (r, RightCrumb n l:bs)

-- | Allows us to navigate up in the tree.
goUp :: (BTree a, Breadcrumbs a) -> (BTree a, Breadcrumbs a)
goUp (t, LeftCrumb n r : bs) = (BNode n t r, bs)
goUp (t, RightCrumb n l : bs) = (BNode n l t, bs)

-- | The zipper is the pair made of the sub-tree (left or right) and
-- the collection of the crumbs to subtree's parent node. The last crumb
-- points to the parent node and the other sub-tree.
type Zipper a = (BTree a, Breadcrumbs a)
```


## About this Code

> You can contact me at veminovici@hotmail.com. Code designed and written in Päädu, on the beautiful island of [**Saaremaa**](https://goo.gl/maps/DmB9ewY2R3sPGFnTA), Estonia.
