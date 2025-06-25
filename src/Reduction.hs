{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-
lambda calculus:
strip-lemma
-}

module Reduction where

import qualified Defs (limit)
import LambdaTerm (LambdaTerm (..))
import Utils.NamedTerm (extractFreeVars)
import qualified Utils.Substitution as Sub

type LambdaFunction = LambdaTerm -> LambdaTerm

-- ???
alphaConvert :: String -> String -> LambdaFunction
alphaConvert = undefined

-- i am not sure if this is how beta reduction should be implemented :D
-- lots of testing incoming

-- normal evalutation stategy -> outermost beta redexes are reduced first
beta1Reduce :: LambdaFunction
beta1Reduce (Application (Abstraction var body) term) =
  Sub.replaceInTerm [] (Variable var) term body
beta1Reduce t@(Variable _) = t
beta1Reduce (Abstraction var body) =
  Abstraction var (beta1Reduce body)
beta1Reduce (Application lhs rhs) =
  Application (beta1Reduce lhs) (beta1Reduce rhs)

-- strict evaluation strategy -> innermost beta redexes are reduced first
beta1ReduceStrict :: LambdaFunction
beta1ReduceStrict = snd . helper False
  where
    helper :: Bool -> LambdaTerm -> (Bool, LambdaTerm)
    helper flag t@(Variable _) = (flag, t)
    helper flag (Abstraction var body) =
      (resflag, Abstraction var resterm)
      where
        (resflag, resterm) = helper flag body
    helper flag (Application t@(Abstraction var body) term) =
      if resflag1 || resflag2
        then (True, Application resterm1 resterm2)
        else (True, Sub.replaceInTerm [] (Variable var) term body)
      where
        (resflag1, resterm1) = helper flag t
        (resflag2, resterm2) = helper flag term
    helper flag (Application lhs rhs) =
      (resflag1 || resflag2, Application resterm1 resterm2)
      where
        (resflag1, resterm1) = helper flag lhs
        (resflag2, resterm2) = helper flag rhs

-- could be optimised ever so slightly by not using fixpoint :)
betaReduce :: LambdaFunction
betaReduce term = helper 0 $ Sub.alphaConvert term
  where
    helper :: Int -> LambdaFunction
    helper n term
      | n > Defs.limit = term
      | Sub.fixpoint beta1Reduce term = term
      | otherwise = helper (n + 1) (Sub.alphaConvert $ beta1Reduce term)

betaReduceStrict :: LambdaFunction
betaReduceStrict = helper 0
  where
    helper :: Int -> LambdaFunction
    helper n term
      | n > Defs.limit = term
      | Sub.fixpoint beta1ReduceStrict term = term
      | otherwise = helper (n + 1) (beta1Reduce term)

-- seems to work :D
etaReduce1 :: LambdaFunction
etaReduce1 = snd . helper ""
  where
    helper :: String -> LambdaTerm -> (Bool, LambdaTerm)
    helper v (Abstraction var body)
      | null v && flag = res
      | otherwise = (flag, Abstraction var rest)
      where
        res@(flag, rest) = if null v then helper var body else helper v body
    helper v term@(Application lhs (Variable var)) =
      if v == var && v `notElem` extractFreeVars lhs
        then (True, lhs)
        else (False, term)
    helper _ t = (False, t)

-- messed up something :D
-- UPDATE: i don't remember if i actually fixed whatever it was that was wrong,
-- but i'll see that as i go (type inference awaits me)
etaReduce :: LambdaFunction
etaReduce = Sub.repeatWhile (==) etaReduce1
