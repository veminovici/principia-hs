module MaybeT where

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Functor (MaybeT m) where
    -- fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
    fmap f x = MaybeT $ do
        a <- runMaybeT x
        return (fmap f a)

instance Monad m => Applicative (MaybeT m) where
    pure = MaybeT . return . Just
    f <*> a = MaybeT $ do
        maybe_f <- runMaybeT f
        maybe_a <- runMaybeT a
        return (maybe_f <*> maybe_a)

instance Monad m => Monad (MaybeT m) where
    return = MaybeT . return . Just
    -- (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
    x >>= f = MaybeT $ do
        maybe_value <- runMaybeT x
        case maybe_value of
            Nothing -> return Nothing
            Just value -> runMaybeT $ f value
