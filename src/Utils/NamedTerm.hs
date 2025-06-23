module Utils.NamedTerm where

import Data.Functor ((<&>))
import qualified Data.Set as S
import Data.Unique (hashUnique, newUnique)
import LambdaTerm (LambdaTerm (..))
import qualified NamelessTerm as Nameless

-- these are kind of weird since the same variable
-- can be both bound and free across the ENTIRE scope of some term
extractFreeVars :: LambdaTerm -> [String]
extractFreeVars = snd . helper [] []
  where
    helper :: [String] -> [String] -> LambdaTerm -> ([String], [String])
    helper bound free (Variable x) =
      if x `elem` bound || x `elem` free
        then (bound, free)
        else (bound, x : free)
    helper bound free (Application t1 t2) =
      (bound2, free2)
      where
        (bound1, free1) = helper bound free t1
        (bound2, free2) = helper bound1 free1 t2
    helper bound free (Abstraction var body) =
      helper (var : bound) free body

extractBoundVars :: LambdaTerm -> [String]
extractBoundVars (Variable _) = []
extractBoundVars (Application lhs rhs) = extractBoundVars lhs ++ extractBoundVars rhs
extractBoundVars (Abstraction var body) = var : extractBoundVars body

extractBound :: LambdaTerm -> S.Set String
extractBound (Variable _) = S.empty
extractBound (Application lhs rhs) = extractBound lhs `S.union` extractBound rhs
extractBound (Abstraction var body) = S.fromList [var] `S.union` extractBound body

genSym :: String -> IO String
genSym var = newUnique <&> ((var ++) . show . hashUnique)

alphaEquivalent :: LambdaTerm -> LambdaTerm -> Bool
alphaEquivalent lhs rhs = Nameless.fromNamed lhs == Nameless.fromNamed rhs

data Bound

data Free

-- phantom type!
type Variables a = [String]

-- lambda lifting

liftMaximal :: LambdaTerm -> LambdaTerm
liftMaximal = helper
  where
    helper t@(Variable x) =
      Abstraction x t
    helper (Application t1 t2) =
      Application (helper t1) (helper t2)
    helper (Abstraction var body@(Abstraction _ _)) =
      Abstraction var $ helper body
    helper t@(Abstraction var body) =
      Abstraction var foldedTerm
      where
        free = reverse $ extractFreeVars t
        foldedTerm = foldr Abstraction body free

liftMinimal :: LambdaTerm -> LambdaTerm
liftMinimal = (\(_, _, x) -> x) . helper [] []
  where
    helper :: Variables Bound -> [Variables Free] -> LambdaTerm -> (Variables Bound, [Variables Free], LambdaTerm)
    helper bv fvc t@(Variable x)
      | null fvc = (bv, fvc, t)
      | x `elem` bv || x `elem` head fvc = (bv, fvc, t)
      | otherwise = (bv, (x : head fvc) : tail fvc, t)
    helper bv fvc (Application t1 t2) =
      (bv, fvc2, Application t11 t22)
      where
        (_, fvc1, t11) = helper bv fvc t1
        (_, fvc2, t22) = helper bv fvc1 t2
    helper bv fvc (Abstraction var body@(Abstraction _ _)) =
      (bv1, fvc1, Abstraction var body1)
      where
        (bv1, fvc1, body1) = helper (var : bv) fvc body
    helper bv fvc (Abstraction var body) =
      if null free
        then (bv, rest, Abstraction var body1)
        else (bv, rest, Abstraction var foldedTerm)
      where
        (_, fvc1, body1) = helper (var : bv) ([] : fvc) body
        free = head fvc1
        rest = tail fvc1
        foldedTerm = foldr Abstraction body1 $ reverse free

-- check if lhs is a subterm of rhs, given the following definition of a subterm:
-- M <= N (M is a subterm of N) iff Sub(M) <= Sub(N) (let's pretend that this is the subset relation symbol)
-- where
-- Sub(x) = {x}
-- Sub(MN) = Sub(M) v Sub(N) v {MN}
-- Sub(\x.M) = Sub(M) v {\x.M}
subterm :: LambdaTerm -> LambdaTerm -> Bool
subterm lhs rhs@(Variable _) = lhs == rhs
subterm lhs rhs@(Application t1 t2) =
  lhs == rhs || subterm lhs t1 || subterm lhs t2
subterm lhs rhs@(Abstraction var body) =
  lhs == rhs || subterm lhs body
