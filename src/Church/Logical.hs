module Church.Logical where

import LambdaTerm (LambdaTerm (..))
import Prelude hiding (and, not, or)

-- plus booleans ig

true :: LambdaTerm
true = Abstraction "x" $ Abstraction "y" $ Variable "x"

false :: LambdaTerm
false = Abstraction "x" $ Abstraction "y" $ Variable "y"

and :: LambdaTerm
and = Abstraction "p" $ Abstraction "q" $ Application (Application (Variable "p") (Variable "q")) (Variable "p")

or :: LambdaTerm
or = Abstraction "p" $ Abstraction "q" $ Application (Application (Variable "p") (Variable "p")) (Variable "q")

not :: LambdaTerm
not = Abstraction "p" $ Application (Application (Variable "p") false) true

cases :: LambdaTerm
cases = Abstraction "p" $ Abstraction "x" $ Abstraction "y" $ Application (Application (Variable "p") (Variable "x")) (Variable "y")

allLogical :: [(String, LambdaTerm)]
allLogical =
  [ ("true", true),
    ("false", false),
    ("and", and),
    ("or", or),
    ("not", not),
    ("cases", cases)
  ]
