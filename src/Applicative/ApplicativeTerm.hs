module ApplicativeTerm where

import LambdaTerm (LambdaTerm (..))
import Utils.NamedTerm

data ApplicativeTerm
  = AVar String
  | AApp ApplicativeTerm ApplicativeTerm
  | S
  | K
  deriving (Show, Eq, Ord)

{-
2.18 - basically the same as SKI calculus but without the I
I = ... (i need to write this out ON PAPER)
but just substitute  I with it

taken very much from wikipedia,
i was able to derive only the first two XD
+ the going from inside out but yeah
\x.x -> I
\x.y -> Ky
\x.Mx -> M if x not free in M (eta-reduction)
\x.y z -> S (\x.y) (\x.z)
-}

skId :: ApplicativeTerm
skId = AApp (AApp S K) K

fromLambda :: LambdaTerm -> ApplicativeTerm
fromLambda (Variable x) = AVar x
fromLambda (Application lhs rhs) =
  AApp (fromLambda lhs) (fromLambda rhs)
fromLambda (Abstraction var (Variable x))
  -- \x.x
  | var == x = skId
  | otherwise = AApp K (AVar x)
fromLambda (Abstraction var (Application lhs rhs@(Variable x)))
  | x `notElem` extractFreeVars lhs = fromLambda lhs
  | otherwise = AApp (AApp S (fromLambda absl)) (fromLambda absr)
  where
    absl = Abstraction var lhs
    absr = Abstraction var rhs
fromLambda (Abstraction var (Application lhs rhs)) =
  AApp (AApp S (fromLambda absl)) (fromLambda absr)
  where
    absl = Abstraction var lhs
    absr = Abstraction var rhs
fromLambda (Abstraction var body) =
  AApp K $ fromLambda body
