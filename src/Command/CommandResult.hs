{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module CommandResult where

import Builtin.Environment
import CommandHelp
import CommandParser (environment)
import qualified CommandParser as CP
import Data.Char (isDigit)
import qualified Data.Map as M
import Defs
import Distribution.Types.VersionRange (intersectVersionRanges)
import Expansion
import LambdaParser
import LambdaTerm
import qualified LocallyNamelessTerm as LNameless
import qualified NamelessTerm as Nameless
import Parser (Parseable (..))
import qualified Reduction as R
import System.Process (CreateProcess (env))
import qualified Utils.NamedTerm as N
import qualified Utils.Substitution as Sub

none :: String -> Bool
none = (== "")

help :: String -> Bool
help = (== "-help")

vis :: String -> Bool
vis = (== "-vis")

num :: String -> Bool
num "" = False
num (x : xs) = x == '-' && all isDigit xs

invalidFlag :: String -> String
invalidFlag cmd = "invalid flag for command " ++ cmd

-- could've maybe used Either String ()? i am tired atp
evaluateInEnv' :: String -> Environment -> (String, Environment)
evaluateInEnv' iden env =
  case M.lookup iden env of
    Nothing -> ("key " ++ iden ++ " is missing from the global environment", env)
    Just t -> case t of
      Left uneval -> case parse uneval of
        Left perror -> (perror, M.delete iden env)
        Right eval -> ("", M.insert iden (Right eval) env)
      Right _ -> ("", env)

evaluateInEnv :: [String] -> Environment -> (String, Environment)
evaluateInEnv tokens env = helper (filter (\tkn -> not (null tkn) && head tkn == '$') tokens) env
  where
    helper [] env = ("", env)
    helper (t : ts) env =
      let (msg, env') = evaluateInEnv' (tail t) env
       in case msg of
            [] -> evaluateInEnv ts env'
            _ -> (msg, env) -- technically env' would be ok too

tokenise :: String -> [String]
tokenise = words

pureToOutput :: forall a b. (Show a, Parseable a, Show b) => String -> Environment -> (a -> b) -> String
pureToOutput input env f =
  case parse @a (expand input env) of
    Left err -> err
    Right term -> show @b $ f term

-- this should potentially be extended to also return a term
-- in the case of a recursive grammar, i.e. term := :beta <t1> <t2> ...
data CommandResult
  = NoOp
  | Output String
  | Error String
  | UpdateEnv (Environment -> (String, Environment))
  | StepThrough String [LambdaTerm] -- potentially polymorphic :)
  | RequireInput ([String] -> CommandResult) -- continuations!
  | Compound [CommandResult]

-- looooooooooooooooooooots and looooooooooooooots of boilerplate code
interpretCommand :: Environment -> CP.Command -> CommandResult
interpretCommand env (CP.Echo flag input)
  | none flag = Output $ expand input env
  | help flag = Output echoHelp
  | otherwise = Error $ invalidFlag "echo"
interpretCommand _ (CP.Browse flag)
  | none flag = NoOp -- should probably be an IO action
  | help flag = Output browseHelp
  | otherwise = Error $ invalidFlag "browse"
interpretCommand env (CP.Env flag)
  | none flag = Output $ showEnv env
  | help flag = Output envHelp
  | otherwise = Error $ invalidFlag "env"
interpretCommand env (CP.Evaluate flag input)
  | none flag = Compound [UpdateEnv $ evaluateInEnv (tokenise input), Output $ pureToOutput @LambdaTerm input env id]
  | help flag = Output evalHelp
  | otherwise = Error $ invalidFlag "eval"
interpretCommand _ (CP.Definition iden input) =
  UpdateEnv (\env -> ("\"" ++ iden ++ "\"" ++ " added to global environment", M.insert iden (Left input) env))
interpretCommand env (CP.Substitute flag old new term)
  | none flag =
      Compound
        [ UpdateEnv $ evaluateInEnv (tokenise old ++ tokenise new ++ tokenise term),
          Output "this is a tad bit more complex (i am lazy)"
        ]
  | help flag = Output substituteHelp
interpretCommand env (CP.AlphaConvert flag input)
  | none flag = Compound [UpdateEnv $ evaluateInEnv (tokenise input), Output $ pureToOutput input env Sub.alphaConvert]
  | help flag = Output alphaHelp
  | otherwise = Error $ invalidFlag "alpha"
interpretCommand env (CP.AlphaEquiv flag lhs rhs)
  | none flag = undefined
  | help flag = Output alphaEqHelp
  | otherwise = Error $ invalidFlag "alphaeq"
interpretCommand env (CP.BetaReduce flag input)
  | none flag = Compound [UpdateEnv $ evaluateInEnv (tokenise input), Output $ pureToOutput input env R.betaReduce]
  | help flag = Output betaHelp
  | num flag = Compound [UpdateEnv $ evaluateInEnv (tokenise input), Output $ pureToOutput input env nthIter]
  | vis flag = Compound [UpdateEnv $ evaluateInEnv (tokenise input), next]
  | otherwise = Error $ invalidFlag "beta"
  where
    nthIter = (!! (read (tail flag) :: Int)) . iterate R.beta1Reduce
    term = parse (expand input env) :: Either String LambdaTerm
    next = case term of
      Left err -> Output err
      Right term -> StepThrough "β> " $ iterate R.beta1Reduce term
interpretCommand env (CP.Beta1Reduce flag input)
  | none flag = Compound [UpdateEnv $ evaluateInEnv (tokenise input), Output $ pureToOutput input env R.beta1Reduce]
  | help flag = Output beta1Help
  | otherwise = Error $ invalidFlag "beta1"
interpretCommand env (CP.BetaReduceStrict flag input)
  | none flag = Compound [UpdateEnv $ evaluateInEnv (tokenise input), Output $ pureToOutput input env R.betaReduceStrict]
  | help flag = Output betaSHelp
  | num flag = Compound [UpdateEnv $ evaluateInEnv (tokenise input), Output $ pureToOutput input env nthIter]
  | vis flag = Compound [UpdateEnv $ evaluateInEnv (tokenise input), next]
  | otherwise = Error $ invalidFlag "betas"
  where
    nthIter = (!! (read (tail flag) :: Int)) . iterate R.beta1ReduceStrict
    term = parse (expand input env) :: Either String LambdaTerm
    next = case term of
      Left err -> Output err
      Right term -> StepThrough "βs> " $ iterate R.beta1ReduceStrict term
interpretCommand env (CP.Beta1ReduceStrict flag input)
  | none flag = Compound [UpdateEnv $ evaluateInEnv (tokenise input), Output $ pureToOutput input env R.beta1ReduceStrict]
  | help flag = Output beta1SHelp
  | otherwise = Error $ invalidFlag "beta1s"
interpretCommand env (CP.EtaReduce flag input)
  | none flag = Compound [UpdateEnv $ evaluateInEnv (tokenise input), Output $ pureToOutput input env R.etaReduce]
  | help flag = Output etaHelp
  | num flag = Compound [UpdateEnv $ evaluateInEnv (tokenise input), Output $ pureToOutput input env nthIter]
  | vis flag = Compound [UpdateEnv $ evaluateInEnv (tokenise input), next]
  | otherwise = Error $ invalidFlag "eta"
  where
    nthIter = (!! (read (tail flag) :: Int)) . iterate R.etaReduce1
    term = parse (expand input env) :: Either String LambdaTerm
    next = case term of
      Left err -> Output err
      Right term -> StepThrough "η> " $ iterate R.etaReduce1 term
interpretCommand env (CP.Eta1Reduce flag input)
  | none flag = Compound [UpdateEnv $ evaluateInEnv (tokenise input), Output $ pureToOutput input env R.etaReduce1]
  | help flag = Output eta1Help
  | otherwise = Error $ invalidFlag "eta1"
interpretCommand env (CP.LiftMaximal flag input)
  | none flag = Compound [UpdateEnv $ evaluateInEnv (tokenise input), Output $ pureToOutput input env N.liftMaximal]
  | help flag = Output liftmaxHelp
  | otherwise = Error $ invalidFlag "liftmax"
interpretCommand env (CP.LiftMinimal flag input)
  | none flag = Compound [UpdateEnv $ evaluateInEnv (tokenise input), Output $ pureToOutput input env N.liftMinimal]
  | help flag = Output liftminHelp
  | otherwise = Error $ invalidFlag "liftmin"
interpretCommand env (CP.Nameless flag input)
  | none flag = Compound [UpdateEnv $ evaluateInEnv (tokenise input), Output $ pureToOutput input env Nameless.fromNamed]
  | help flag = Output namelessHelp
  | otherwise = Error $ invalidFlag "nameless"
interpretCommand env (CP.LocallyNameless flag input)
  | none flag = Compound [UpdateEnv $ evaluateInEnv (tokenise input), Output $ pureToOutput input env LNameless.fromNamed]
  | help flag = Output locallyNamelessHelp
  | otherwise = Error $ invalidFlag "lnameless"
interpretCommand env (CP.Infer flag input)
  | none flag = Compound [UpdateEnv $ evaluateInEnv (tokenise input), Output "todo"]
  | help flag = Output inferHelp
  | otherwise = Error $ invalidFlag "infer (:t)"
interpretCommand _ _ = error "oops, forgot to implement"
