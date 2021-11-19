module LambdaCalculus where

type Info = String
type NameBinding = String
type Context = (String, NameBinding)
type ContextLength = Int 

data Term
    = TmVar Info Int ContextLength
    | TmAbs Info String Term
    | TmApp Info Term Term
    deriving Show

shiftTerm :: Int -> Term -> Term
shiftTerm d = walk 0
    where
        walk c (TmVar i x n) | x >=c = TmVar i (x + d) (n + d)
        walk c (TmVar i x n)         = TmVar i x (n + d)
        walk c (TmAbs i s t1)        = TmAbs i s (walk (c + 1) t1)
        walk c (TmApp i t1 t2)       = TmApp i (walk c t1) (walk c t2)

-- printtm :: Context -> Term -> String
-- printtm ctx t =
--     case t of
--         TmAbs _ x t1 -> let (ctx', x') = pickFreshName ctx x in "(lambda " ++ show x' ++ ". " ++ printtm ctx' t1 ++ ")"
--         TmApp _ t1 t2 -> "(" ++ printtm ctx t1 ++ " " ++ printtm ctx t2 ++ ")"
--         TmVar i x n -> 
--             if ctxlength ctx == n then
--                 index2name i ctx x
--             else
--                 "bad index"
