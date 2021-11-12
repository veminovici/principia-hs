module Writer where

newtype Writer log a = Writer { runWriter :: (log, a) }

instance Functor (Writer log) where
    fmap f (Writer (log, a)) = Writer (log, f a)

instance Monoid log => Applicative (Writer log) where
    pure x = Writer (mempty, x)
    (Writer (lf, f)) <*> (Writer (la, a)) = Writer (lf <> la, f a)

instance Monoid log => Monad (Writer log) where
    return = pure
    Writer (la, a) >>= f = 
        let Writer (lb, b) = f a
        in Writer (la <> lb, b)

-- | Appends to the log.
appendLog :: Monoid log => log -> Writer log a -> Writer log a
appendLog l (Writer (la, a)) = Writer (la <> l, a)

-- | Sets the internal log to a given value
tell :: log -> Writer log ()
tell log = Writer (log, ())

-- | Modified the internal log.
censor :: (log -> log) -> Writer log a -> Writer log a
censor f (Writer (l, a)) = Writer (f l, a)

-- | Returns the current log along with the given value.
listen :: Writer log a -> Writer log (a, log)
listen (Writer (l, a)) = Writer (l, (a, l))
