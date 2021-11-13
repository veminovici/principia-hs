module Zippers where

data BTree a = BEmpty | BNode a (BTree a) (BTree a) deriving (Show)
data Direction = L | R deriving(Show)
type Directions = [Direction]

change :: a -> Directions -> BTree a -> BTree a
change v (L : ds) (BNode n l r) = BNode n (change v ds l) r
change v (R : ds) (BNode n l r) = BNode n l (change v ds r)
change v [] (BNode _ l r) = BNode v l r
change v [] BEmpty = BNode v BEmpty BEmpty

elementAt :: Directions -> BTree a -> Maybe a
elementAt (L:ds) (BNode n l r) = elementAt ds l
elementAt (R:ds) (BNode n l r) = elementAt ds r
elementAt [] (BNode n _ _) = Just n
elementAt [] BEmpty = Nothing

data Crumb a = LeftCrumb a (BTree a) | RightCrumb a (BTree a)
type Breadcrumbs a = [Crumb a]

goLeft :: (BTree a, Breadcrumbs a) -> (BTree a, Breadcrumbs a)
goLeft (BNode n l r, bs) = (l, LeftCrumb n r:bs)

goRight :: (BTree a, Breadcrumbs a) -> (BTree a, Breadcrumbs a)
goRight (BNode n l r, bs) = (r, RightCrumb n l:bs)

goUp :: (BTree a, Breadcrumbs a) -> (BTree a, Breadcrumbs a)
goUp (t, LeftCrumb n r : bs) = (BNode n t r, bs)
goUp (t, RightCrumb n l : bs) = (BNode n l t, bs)


type Zipper a = (BTree a, Breadcrumbs a)
