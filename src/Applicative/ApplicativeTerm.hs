{-# LANGUAGE InstanceSigs #-}

module ApplicativeTerm where

import LambdaTerm (LambdaTerm (..))
import Reduction
import qualified Utils.NamedTerm as N

data ApplicativeTerm
  = AVar String
  | AApp ApplicativeTerm ApplicativeTerm
  | S
  | K
  deriving (Eq, Ord)

instance Show ApplicativeTerm where
  show :: ApplicativeTerm -> String
  show (AVar x) = x
  show (AApp lhs rhs@(AApp _ _)) =
    show lhs ++ " (" ++ show rhs ++ ")"
  show (AApp lhs rhs) = show lhs ++ " " ++ show rhs
  show S = "s"
  show K = "k"

skId :: ApplicativeTerm
skId = AApp (AApp S K) K

-- a lot more straightforward :)
isFreeIn :: String -> ApplicativeTerm -> Bool
var `isFreeIn` (AVar x) = var == x
var `isFreeIn` (AApp lhs rhs) = var `isFreeIn` lhs || var `isFreeIn` rhs
_ `isFreeIn` _ = False

-- doesn't eta reduce the term. it's trivial to implement
-- but fuck that. this is horrible for testing?
-- the SK-encoding of the S-combinator is hella long?
fromLambda :: LambdaTerm -> ApplicativeTerm
fromLambda (Variable x) = AVar x
fromLambda (Application lhs rhs) = AApp (fromLambda lhs) (fromLambda rhs)
fromLambda (Abstraction var body@(Application lhs rhs))
  | var `N.isFreeIn` body = AApp (AApp S (fromLambda' var (fromLambda lhs))) (fromLambda' var (fromLambda rhs))
  | otherwise = AApp K (fromLambda body)
fromLambda (Abstraction var body)
  | var `N.isFreeIn` body = fromLambda' var (fromLambda body)
  | otherwise = AApp K (fromLambda body)

fromLambda' :: String -> ApplicativeTerm -> ApplicativeTerm
fromLambda' var t@(AApp lhs rhs)
  | var `isFreeIn` t = AApp (AApp S (fromLambda' var lhs)) (fromLambda' var rhs)
  | otherwise = AApp K t
fromLambda' var t
  | var `isFreeIn` t = skId
  | otherwise = AApp K t
