module State where

newtype State s a = State { runState :: s -> (s, a) }
