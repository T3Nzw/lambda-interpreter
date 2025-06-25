module Church.List where

import Church.Logical
import Church.Numerals
import LambdaTerm
import Prelude hiding (filter, foldr, length, map)

-- \c n.n
nil :: LambdaTerm
nil = Abstraction "c" $ Abstraction "n" $ Variable "n"

-- \h t c n.c h (t c n)
cons :: LambdaTerm
cons =
  Abstraction "h" $
    Abstraction "t" $
      Abstraction "c" $
        Abstraction "n" $
          Application
            (Application (Variable "c") (Variable "h"))
            (Application (Application (Variable "t") (Variable "c")) (Variable "n"))

length :: LambdaTerm
length = Abstraction "l" $ Application (Application (Variable "l") (Abstraction "x" Church.Numerals.succ)) zero

append :: LambdaTerm
append =
  Abstraction "l" $
    Abstraction "r" $
      Abstraction "c" $
        Abstraction "n" $
          Application
            (Application (Variable "l") (Variable "c"))
            ( Application
                (Application (Variable "r") (Variable "c"))
                (Variable "n")
            )

member :: LambdaTerm
member =
  Abstraction "p" $
    Abstraction "l" $
      Application
        ( Application
            (Variable "l")
            ( Abstraction "x" $
                Abstraction "a" $
                  Application
                    ( Application
                        Church.Logical.or
                        (Application (Variable "p") (Variable "x"))
                    )
                    (Variable "a")
            )
        )
        false

map :: LambdaTerm
map =
  Abstraction "f" $
    Abstraction "l" $
      Abstraction "c" $
        Abstraction "n" $
          Application
            ( Application
                (Variable "l")
                ( Abstraction "x" $
                    Abstraction "a" $
                      Application
                        ( Application
                            (Variable "c")
                            (Application (Variable "f") (Variable "x"))
                        )
                        (Variable "a")
                )
            )
            (Variable "n")

filter :: LambdaTerm
filter =
  Abstraction "p" $
    Abstraction "l" $
      Abstraction "c" $
        Abstraction "n" $
          Application
            ( Application
                (Variable "l")
                ( Abstraction "n" $
                    Abstraction "a" $
                      Application
                        ( Application
                            ( Application
                                cases
                                (Application (Variable "p") (Variable "n"))
                            )
                            ( Application
                                (Application (Variable "c") (Variable "n"))
                                (Variable "a")
                            )
                        )
                        (Variable "a")
                )
            )
            (Variable "n")

foldr :: LambdaTerm
foldr =
  Abstraction "c" $
    Abstraction "n" $
      Abstraction "l" $
        Application
          (Application (Variable "l") (Variable "c"))
          (Variable "n")

allList :: [(String, LambdaTerm)]
allList =
  [ ("nil", nil),
    ("cons", cons),
    ("length", length),
    ("append", append),
    ("member", member),
    ("map", map),
    ("filter", filter),
    ("foldr", foldr)
  ]
