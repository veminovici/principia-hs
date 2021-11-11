{-# LANGUAGE TupleSections #-}

module State where

newtype State s a = State { runState :: s -> (s, a) }

instance Functor (State s) where
    fmap f (State sa) = State { runState = \s -> 
        let (s', a) = sa s 
        in (s', f a) }

instance Applicative (State s) where
    pure x = State { runState = (, x) }
    (State sf) <*> (State sa) = State { runState = \s ->
        let (s', f) = sf s
            (s'', a) = sa s'
            in (s'', f a) }

instance Monad (State s) where
    return = pure
    (State sa) >>= f = State { runState = \s -> 
        let (s', a) = sa s
            (State sb) = f a
            in sb s' }

-- | Returns the current internal state
get :: State s s
get = State { runState = \s -> (s, s) }

-- | Sets the internal state
put :: s -> State s ()
put s = State { runState = const (s, ()) }

-- | Updates the internal state using the given function
modify :: (s -> s) -> State s () 
modify f = get >>= put . f
