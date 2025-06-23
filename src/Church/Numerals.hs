module Church.Numerals where

import LambdaTerm

zero :: LambdaTerm
zero = Abstraction "f" $ Abstraction "x" $ Variable "x"

one :: LambdaTerm
one = Abstraction "f" $ Abstraction "x" $ Application (Variable "f") (Variable "x")

succ :: LambdaTerm
succ = Abstraction "n" $ Abstraction "f" $ Abstraction "x" $ Application (Variable "f") (Application (Application (Variable "n") (Variable "f")) (Variable "x"))

add :: LambdaTerm
add = Abstraction "m" $ Abstraction "n" $ Abstraction "f" $ Abstraction "x" $ Application (Application (Variable "m") (Variable "n")) (Application (Variable "f") (Variable "x"))

allNumerals :: [(String, LambdaTerm)]
allNumerals =
  [ ("zero", zero),
    ("one", one),
    ("succ", Church.Numerals.succ),
    ("add", add)
  ]

-- assert the term is not ill-formed :)
toInt :: LambdaTerm -> Maybe Int
toInt (Abstraction f (Abstraction x body)) = helper 0 f x body
  where
    helper :: Int -> String -> String -> LambdaTerm -> Maybe Int
    helper n _ x (Variable y) = if x == y then Just n else Nothing
    helper n f x (Application (Variable g) body) =
      if f == g
        then helper (n + 1) f x body
        else Nothing
    helper _ _ _ _ = Nothing
toInt _ = Nothing
