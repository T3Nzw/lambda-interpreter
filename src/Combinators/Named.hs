{-# HLINT ignore "Use camelCase" #-}

module Combinators.Named where

import LambdaTerm

c_I :: LambdaTerm
c_I = Abstraction "x" $ Variable "x"

c_K :: LambdaTerm
c_K = Abstraction "x" $ Abstraction "y" $ Variable "x"

c_KS :: LambdaTerm
c_KS = Abstraction "x" $ Abstraction "y" $ Variable "y"

c_S :: LambdaTerm
c_S = Abstraction "x" $ Abstraction "y" $ Abstraction "z" $ Application (Application (Variable "x") (Variable "z")) (Application (Variable "y") (Variable "z"))

c_omega :: LambdaTerm
c_omega = Abstraction "x" $ Application (Variable "x") (Variable "x")

c_OMEGA :: LambdaTerm
c_OMEGA = Application c_omega c_omega

c_Y :: LambdaTerm
c_Y = Abstraction "f" $ Application inner inner
  where
    inner = Abstraction "x" $ Application (Variable "f") (Application (Variable "x") (Variable "x"))

allCombinators :: [(String, LambdaTerm)]
allCombinators =
  [ ("I", c_I),
    ("K", c_K),
    ("KS", c_KS),
    ("S", c_S),
    ("omega", c_omega),
    ("OMEGA", c_OMEGA),
    ("Y", c_Y)
  ]
