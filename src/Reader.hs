module Reader where

newtype Reader c a = Reader { runReader :: c -> a }

instance Functor (Reader c) where
    fmap f (Reader ca) = Reader { runReader = \c -> 
        let a = ca c in f a }

instance Applicative (Reader c) where
    pure x = Reader { runReader = const x }
    (Reader cf) <*> (Reader ca) = Reader { runReader = \c -> 
        let f = cf c
            a = ca c
            in f a }

instance Monad (Reader c) where
    return = pure
    (Reader ca) >>= f = Reader { runReader = \c ->
        let a = ca c
            (Reader cb) = f a
            in cb c }

-- | Creates a reader that returns the configuration
ask :: Reader c c
ask = Reader { runReader = id }

-- | Creates a reader that returns the value returns by a function.
asks :: (c -> a) -> Reader c a
asks f = Reader { runReader = f }
