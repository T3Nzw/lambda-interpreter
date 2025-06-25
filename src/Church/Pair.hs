module Church.Pair where

import LambdaTerm

pair :: LambdaTerm
pair = Abstraction "x" $ Abstraction "y" $ Abstraction "f" $ Application (Application (Variable "f") (Variable "x")) (Variable "y")

leftproj :: LambdaTerm
leftproj = Abstraction "p" $ Application (Variable "p") $ Abstraction "x" $ Abstraction "y" $ Variable "x"

rightproj :: LambdaTerm
rightproj = Abstraction "p" $ Application (Variable "p") $ Abstraction "x" $ Abstraction "y" $ Variable "y"

leftapply :: LambdaTerm
leftapply =
  Abstraction "g" $
    Abstraction "x" $
      Abstraction "y" $
        Abstraction "f" $
          Application (Application (Variable "f") (Application (Variable "g") (Variable "x"))) (Variable "y")

rightapply :: LambdaTerm
rightapply =
  Abstraction "g" $
    Abstraction "x" $
      Abstraction "y" $
        Abstraction "f" $
          Application (Application (Variable "f") (Variable "x")) (Application (Variable "g") (Variable "y"))

allPairs :: [(String, LambdaTerm)]
allPairs =
  [ ("pair", pair),
    ("leftproj", leftproj),
    ("rightproj", rightproj),
    ("leftapply", leftapply),
    ("rightapply", rightapply)
  ]
