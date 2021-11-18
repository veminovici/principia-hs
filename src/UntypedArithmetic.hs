module UntypedArithmetic where

-- | The extra information attached to a node (eg. source file, line number)
type Info = String
type Err = String

data Term
    = TmFalse Info
    | TmTrue Info
    | TmIf Info Term Term Term
    | TmZero Info
    | TmSucc Info Term
    | TmPred Info Term
    | TmIsZero Info Term
    | TmException Err
    deriving Show

-- | Returns True if the term is a numerical value.
isNumericalVal :: Term -> Bool
isNumericalVal (TmZero _) = True
isNumericalVal (TmSucc _ t) = isNumericalVal t
isNumericalVal _ = False

-- | Returns True if the term is a value.
isVal :: Term -> Bool
isVal (TmFalse _) = True
isVal (TmTrue _) = True
isVal t | isNumericalVal t = True
isVal _ = False

dummyInfo = "" :: Info

-- | One-step evaluation for a term
eval1 :: Term -> Term
eval1 (TmIf _ (TmTrue _) t1 _) = t1
eval1 (TmIf _ (TmFalse _) _ t2) = t2
eval1 (TmIf i t1 t2 t3) = let t1' = eval1 t1 in TmIf i t1' t2 t3
eval1 (TmSucc i t1) = let t1' = eval1 t1 in TmSucc i t1'
eval1 (TmPred _ (TmZero _)) = TmZero dummyInfo
eval1 (TmPred _ (TmSucc _ t)) | isNumericalVal t = t
eval1 (TmPred i t1) = let t1' = eval1 t1 in TmPred i t1'
eval1 (TmIsZero _ (TmZero _)) = TmTrue dummyInfo
eval1 (TmIsZero _ (TmSucc _ t)) | isNumericalVal t = TmFalse dummyInfo
eval1 (TmIsZero i t1) = let t1' = eval1 t1 in TmIsZero i t1'
eval1 _ = TmException "No rule applies"

-- | Full evaluation
eval :: Term -> Term
eval t@(TmException _) = t
eval t = let t' = eval1 t in eval t'
