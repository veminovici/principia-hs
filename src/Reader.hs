module Reader where

newtype Reader c a = Reader { runReader :: c -> a }

instance Functor (Reader c) where
    fmap f (Reader ca) = Reader (\c -> 
        let a = ca c in f a)

instance Applicative (Reader c) where
    pure x = Reader (const x)
    (Reader cf) <*> (Reader ca) = Reader (\c -> 
        let f = cf c
            a = ca c
            in f a)

instance Monad (Reader c) where
    return = pure
    (Reader ca) >>= f = Reader (\c ->
        let a = ca c
            (Reader cb) = f a
            in cb c)

-- | Creates a reader that returns the configuration
ask :: Reader c c
ask = Reader id

-- | Creates a reader that returns the value returns by a function.
asks :: (c -> a) -> Reader c a
asks = Reader

local :: (c -> c') -> Reader c' a -> Reader c a
local f (Reader cc) = Reader (\c ->
    let c' = f c in cc c')
