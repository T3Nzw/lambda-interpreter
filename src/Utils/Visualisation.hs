module Utils.Visualisation where

import LambdaTerm (LambdaTerm (..))
import NamelessTerm (NamelessTerm (..), fromNamed)

-- one day...

eqPrefix :: String
eqPrefix = "\ESC[0m"

diffPrefix :: String
diffPrefix = "\ESC[33m"

colours :: [String]
colours = cycle $ map (\x -> "\ESC[" ++ show x ++ "m") ([31 .. 36] :: [Int])

diffNamed :: LambdaTerm -> LambdaTerm -> (String, String)
diffNamed lhs@(Variable _) rhs@(Variable _)
  | lhs == rhs = (eqPrefix ++ show lhs, eqPrefix ++ show rhs)
  | otherwise = (diffPrefix ++ show lhs, diffPrefix ++ show rhs)
diffNamed (Application lhs1 rhs1) (Application lhs2 rhs2) =
  (s11 ++ s21, s12 ++ s22)
  where
    (s11, s12) = diffNamed lhs1 lhs2
    (s21, s22) = diffNamed rhs1 rhs2
diffNamed lhs@(Abstraction var1 body1) rhs@(Abstraction var2 body2)
  | var1 == var2 = (eqPrefix ++ "λ" ++ var1 ++ "." ++ s1, eqPrefix ++ "λ" ++ var2 ++ "." ++ s2)
  | otherwise = (diffPrefix ++ "λ" ++ var1 ++ "." ++ show lhs, diffPrefix ++ "λ" ++ var2 ++ "." ++ show rhs)
  where
    (s1, s2) = diffNamed body1 body2
diffNamed lhs rhs = (diffPrefix ++ show lhs, diffPrefix ++ show rhs)

diffNameless :: NamelessTerm -> NamelessTerm -> (String, String)
diffNameless = undefined

diffNamedNameless :: LambdaTerm -> (String, String)
diffNamedNameless term = helper term (fromNamed term) colours
  where
    helper :: LambdaTerm -> NamelessTerm -> [String] -> (String, String)
    helper lhs@(Variable _) rhs@(Var _) (colour : _) =
      (colour ++ show lhs, colour ++ show rhs)
    helper (Application lhs1 rhs1) (App lhs2 rhs2) rest1@(_ : rest2) =
      (s11 ++ s21, s12 ++ s22)
      where
        (s11, s12) = helper lhs1 lhs2 rest1
        (s21, s22) = helper rhs1 rhs2 rest2
    helper (Abstraction var body1) (Abs body2) (colour : rest) =
      (colour ++ "λ" ++ var ++ "." ++ s1, colour ++ "λ." ++ s2)
      where
        (s1, s2) = helper body1 body2 rest
    helper _ _ _ = error "i've messed up the de Bruijn indices implementation"

visualise :: (String, String) -> String
visualise (s1, s2) = "\ESC[0m>>> " ++ s1 ++ "\n\ESC[0m>>> " ++ s2
