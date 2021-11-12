module Isomorphism where
import Control.Monad((>=>))

-- | The lens is a pair of functions: a getter and a setter.
data Lens a b = Lens (a -> b) (b -> a -> a)

get :: Lens a b -> (a -> b)
get (Lens g _) = g

set :: Lens a b -> (b -> a -> a)
set (Lens _ s) = s

composeLens :: Lens a b -> Lens b c -> Lens a c
composeLens (Lens g1 s1) (Lens g2 s2) =
    let s = \c a ->
            let b = g1 a
                b' = s2 c b
            in s1 b' a
    in Lens (g2 . g1) s

data Prism a b = Prism (a -> Maybe b) (b -> a -> a)

composePrism :: Lens a b -> Prism b c -> Prism a c
composePrism (Lens g1 s1) (Prism g2 s2) =
    let g = g2 . g1
        s = \c a ->
            let b = g1 a
                b' = s2 c b
            in s1 b' a
    in Prism g s

-- | Isomorphism is a pair of converter functions from one type to a second one. 
-- The conversion only is possible (eg String -> [Char] and [Char] -> String are always possible)
data Isomorphism a b = Iso (a -> b) (b -> a)

-- | Isomorphism is a pair of converter functions from one type to a second one. 
-- The convertion is not only possible (eg. String -> Int is not only possible, while Int -> String is is alway possible)
data Epimorphism a b = Epi (a -> Maybe b) (b -> a)

composeIso :: Isomorphism a b -> Isomorphism b c -> Isomorphism a c
composeIso (Iso fab bab) (Iso fbc bbc) = Iso (fbc . fab) (bab . bbc)

composeEpi (Epi fab bab) (Epi fbc bbc) = Epi (fab >=> bab) (bab . bbc)
