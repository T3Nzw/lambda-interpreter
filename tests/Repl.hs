module Repl where

import Builtin.Environment (builtinEnv, showEnv)
import Church.Numerals
import CommandHelp (envHelp)
import Interpreter (errorMsg, eval)
import LambdaTerm (LambdaTerm (Abstraction, Application, Variable))
import Reduction
import Test.Hspec

-- repl tests
runTests :: IO ()
runTests = hspec $ do
  describe ":env" $ do
    context "when provided with no flags" $ do
      it "can show the starting state of the global environment" $ do
        eval builtinEnv ":env" `shouldBe` showEnv builtinEnv

    context "when provided with the help flag" $ do
      it "can show information about the command :env" $ do
        eval builtinEnv ":env -help" `shouldBe` envHelp

    context "when provided with invalid input" $ do
      it "failes to be parsed and returns an error message" $ do
        eval builtinEnv ":env a" `shouldBe` errorMsg

  describe ":to-int" $ do
    context "when provided with a valid Church numeral" $ do
      it "can convert it to an integer value wrapped in a Maybe monad" $ do
        toInt zero `shouldBe` Just 0
        toInt one `shouldBe` Just 1
        toInt (betaReduce (Application Church.Numerals.succ zero)) `shouldBe` Just 1
        toInt (betaReduce (Application Church.Numerals.succ one)) `shouldBe` Just 2

    context "when provided with an invalid Church numeral" $ do
      it "returns nothing" $ do
        toInt Church.Numerals.succ `shouldBe` Nothing
        toInt (Variable "X") `shouldBe` Nothing
        toInt (Application Church.Numerals.succ one) `shouldBe` Nothing
