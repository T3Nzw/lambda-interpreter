module Nameless where

import Data.Either
import LambdaParser
import LambdaTerm (LambdaTerm (..))
import NamelessTerm (NamelessTerm (..), fromNamed, substitute)
import Parser
import Test.Hspec
import Utils.NamedTerm (alphaEquivalent)

-- this one is a bit nasty
namedTerms :: [LambdaTerm]
namedTerms =
  map
    (fromRight (Variable "error") . parse)
    [ "\\x.x",
      "\\x y.x",
      "\\x y.x y",
      "\\x.y",
      "\\x y.y (\\x.x)",
      "\\x.y z (\\z.z y)"
    ]

namelessTerms :: [NamelessTerm]
namelessTerms = map fromNamed namedTerms

runTests :: IO ()
runTests = hspec $ do
  describe "fromNamed" $ do
    context "when called on a named lambda term" $ do
      it "can properly convert it to a nameless term" $ do
        namelessTerms
          `shouldBe` [ Abs $ Var 0,
                       Abs $ Abs $ Var 1,
                       Abs $ Abs $ App (Var 1) (Var 0),
                       Abs $ Var 1,
                       Abs $ Abs $ App (Var 0) (Abs $ Var 0),
                       Abs $ App (App (Var 1) (Var 2)) (Abs $ App (Var 0) (Var 2))
                     ]

  describe "substitute :: Int -> NamelessTerm -> NamelessTerm -> NamelessTerm" $ do
    context "substitutes an index in a nameless term with another nameless term" $ do
      it "\\.0[0 -> 1]  ==>  \\.0" $ do
        substitute 0 (Var 1) (head namelessTerms) `shouldBe` head namelessTerms

      it "\\.\\.1[0 -> 2]  ==> \\.\\.1" $ do
        substitute 0 (Var 2) (namelessTerms !! 1) `shouldBe` namelessTerms !! 1

      it "\\.\\.1[1 -> 2]  ==>  \\.\\.1" $ do
        substitute 1 (Var 2) (namelessTerms !! 1) `shouldBe` namelessTerms !! 1

      it "\\.1[1 -> 2]  ==>  \\.1" $ do
        substitute 1 (Var 2) (namelessTerms !! 3) `shouldBe` namelessTerms !! 3

      it "\\.1[1 -> 0]  ==>  \\.1" $ do
        substitute 1 (Var 0) (namelessTerms !! 3) `shouldBe` namelessTerms !! 3

  describe "alphaEquivalent :: LambdaTerm -> LambdaTerm -> Bool" $ do
    context "takes two NAMED terms and checks if they are alpha-equivalent using de Bruijn indices" $ do
      it "\\x.x & \\y.y  == true" $ do
        -- parse returns an Either String LambdaTerm value (due to potential errors during parsing),
        -- so i didn't bother unpacking the value
        liftA2 alphaEquivalent (parse "\\x.x") (parse "\\y.y") `shouldBe` Right True
