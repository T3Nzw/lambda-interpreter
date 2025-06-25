{-
So far errors are NOT descriptive as the failure of any parser
will result in an empty list ([]) being returned. This could be
(relatively easily, but it would be very tedious to do) fixed by
just using a monad / monad transformer that allows for a "failure state".
Such a monad would, for example, be Either String (Parser a), or
in the case of a monad transformer, ExceptT
-}

module CommandParser where

import Control.Applicative
import Data.Functor (($>))
import qualified Data.Map as M
import Defs
import LambdaTerm (LambdaTerm)
import Parser

keywords :: [String]
keywords =
  [ "let",
    "="
  ]

validTermSymbols :: [Char]
validTermSymbols = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ " $().\\" ++ validDefinitionSymbols

validDefinitionSymbols :: [Char]
validDefinitionSymbols = "-_?'*^"

commandP :: String -> Parser String
commandP s = spaces >> string s

branchF :: Parser String
branchF = helpEof <|> op

helpEof :: Parser String
helpEof = string "-help" <* (spaces1 <|> eof)

op :: Parser String
op = vis <|> CommandParser.repeat <|> none

vis :: Parser String
vis = string "-vis" <* spaces1

repeat :: Parser String
repeat = string "-" >>= \r -> ((++) r <$> many1 digit) <* spaces1

none :: Parser String
none = string "" <* (eof <|> spaces)

lterm :: Parser String
lterm = many1 (sat (`elem` validTermSymbols))

ltoken :: Parser String
ltoken = token lterm

namelesstoken :: Parser String
namelesstoken = token $ many1 $ digit <|> oneOf "(). \\"

data Command
  = Echo {_flag :: String, _input :: String}
  | Browse {_flag :: String}
  | Env {_flag :: String}
  | Definition
      { _identifier :: String,
        _term :: String
      }
  | Evaluate {_flag :: String, _identifier :: String}
  | Substitute
      { _flag :: String,
        _old :: String,
        _new :: String,
        _term :: String
      }
  | AlphaConvert {_flag :: String, term :: String}
  | AlphaEquiv {_flag :: String, _lhs :: String, _rhs :: String}
  | BetaReduce {_flag :: String, _term :: String}
  | Beta1Reduce {_flag :: String, _term :: String}
  | BetaReduceStrict {_flag :: String, _term :: String}
  | Beta1ReduceStrict {_flag :: String, _term :: String}
  | Eta1Reduce {_flag :: String, _term :: String}
  | EtaReduce {_flag :: String, _term :: String}
  | LiftMaximal {_flag :: String, _term :: String}
  | LiftMinimal {_flag :: String, _term :: String}
  | Nameless {_flag :: String, _term :: String}
  | LocallyNameless {_flag :: String, _term :: String}
  | SubstituteNameless {_flag :: String, _old :: String, _new :: String, _term :: String} -- TODO
  | NumeralToInt {_flag :: String, _term :: String}
  | Applicative {_flag :: String, _term :: String}
  | Named {_flag :: String, _term :: String}
  | Infer {_flag :: String, _term :: String}
  deriving (Show, Eq)

variableName :: Parser String
variableName = token $ many1 $ alphanum <|> sat (`elem` validDefinitionSymbols)

lbranch :: String -> Parser String
lbranch flag = Parser.branch (flag /= "-help") ltoken (many' item)

lbranchEof :: String -> Parser String
lbranchEof flag = (lbranch flag <* spaces) <* eof

nbranch :: String -> Parser String
nbranch flag = Parser.branch (flag /= "-help") namelesstoken (many' item)

nbranchEof :: String -> Parser String
nbranchEof flag = (nbranch flag <* spaces) <* eof

echo :: Parser Command
echo = do
  _ <- commandP ":echo"
  _ <- spaces1 <|> eof
  flag <- CommandParser.branchF
  input <- many' item

  return $ Echo flag input

browse :: Parser Command
browse = do
  _ <- commandP ":browse"
  flag <- (spaces1 >> helpEof) <|> none
  spaces >> eof

  return $ Browse flag

environment :: Parser Command
environment = do
  _ <- commandP ":env"
  flag <- (spaces1 >> helpEof) <|> none
  spaces >> eof

  return $ Env flag

eval :: Parser Command
eval = do
  _ <- commandP ":eval"
  _ <- spaces1
  flag <- branchF
  Evaluate flag <$> lbranchEof flag

define :: Parser Command
define = do
  iden <- token variableName
  _ <- commandP ":="
  term <- ltoken

  if iden `elem` keywords || term `elem` keywords
    then zero
    else return $ Definition iden term

substitute :: Parser Command
substitute = do
  _ <- commandP ":sub+"
  _ <- spaces1
  flag <- branchF
  case flag of
    "-help" -> do
      _ <- many' item -- it doesn't have to consume everything, but it would be theoretically more correct, i reckon?
      return $ Substitute flag "" "" ""
    _ -> subP flag
  where
    subP :: String -> Parser Command
    subP flag = do
      old <- ltoken
      _ <- char ';'
      new <- ltoken
      _ <- char ';'
      term <- ltoken
      eof
      return $ Substitute flag old new term

alphaConvert :: Parser Command
alphaConvert = do
  _ <- commandP ":alpha"
  _ <- spaces1
  flag <- branchF
  AlphaConvert flag <$> lbranchEof flag

alphaEquiv :: Parser Command
alphaEquiv = do
  _ <- commandP ":alphaeq"
  _ <- spaces1
  flag <- branchF
  case flag of
    "-help" -> do
      _ <- many' item
      return $ AlphaEquiv flag "" ""
    _ -> alphaEqP flag
  where
    alphaEqP :: String -> Parser Command
    alphaEqP flag = do
      lhs <- ltoken
      _ <- char ';'
      rhs <- ltoken
      eof
      return $ AlphaEquiv flag lhs rhs

betaReduce :: Parser Command
betaReduce = do
  _ <- commandP ":beta"
  _ <- spaces1
  flag <- branchF
  BetaReduce flag <$> lbranchEof flag

beta1Reduce :: Parser Command
beta1Reduce = do
  _ <- commandP ":beta1"
  _ <- spaces1
  flag <- helpEof <|> op
  Beta1Reduce flag <$> lbranchEof flag

betaReduceStrict :: Parser Command
betaReduceStrict = do
  _ <- commandP ":betas"
  _ <- spaces1
  flag <- branchF
  BetaReduceStrict flag <$> lbranchEof flag

beta1ReduceStrict :: Parser Command
beta1ReduceStrict = do
  _ <- commandP ":beta1s"
  _ <- spaces1
  flag <- branchF
  Beta1ReduceStrict flag <$> lbranchEof flag

betas :: Parser Command
betas = beta1ReduceStrict <|> betaReduceStrict <|> beta1Reduce <|> betaReduce

eta1Reduce :: Parser Command
eta1Reduce = do
  _ <- commandP ":eta1"
  _ <- spaces1
  flag <- branchF
  Eta1Reduce flag <$> lbranchEof flag

etaReduce :: Parser Command
etaReduce = do
  _ <- commandP ":eta"
  _ <- spaces1
  flag <- branchF
  EtaReduce flag <$> lbranchEof flag

liftMaximal :: Parser Command
liftMaximal = do
  _ <- commandP ":liftmax"
  _ <- spaces1
  flag <- branchF
  LiftMaximal flag <$> lbranchEof flag

liftMinimal :: Parser Command
liftMinimal = do
  _ <- commandP ":liftmin"
  _ <- spaces1
  flag <- branchF
  LiftMinimal flag <$> lbranchEof flag

nameless :: Parser Command
nameless = do
  _ <- commandP ":nameless"
  _ <- spaces1
  flag <- branchF
  Nameless flag <$> lbranchEof flag

lnameless :: Parser Command
lnameless = do
  _ <- commandP ":lnameless"
  _ <- spaces1
  flag <- branchF
  LocallyNameless flag <$> lbranchEof flag

numeralToInt :: Parser Command
numeralToInt = do
  _ <- commandP ":to-int"
  _ <- spaces1
  flag <- branchF
  NumeralToInt flag <$> lbranchEof flag

-- TODO: fix validation for nameless terms (cannot contain letters)
subnameless :: Parser Command
subnameless = do
  _ <- commandP ":subnameless"
  _ <- spaces1
  flag <- branchF
  case flag of
    "-help" -> many' item $> SubstituteNameless flag "" "" ""
    _ -> subP flag
  where
    subP flag = do
      old <- namelesstoken
      _ <- char ';'
      new <- namelesstoken
      _ <- char ';'
      term <- namelesstoken
      return $ SubstituteNameless flag old new term

applicative :: Parser Command
applicative = do
  _ <- commandP ":app"
  _ <- spaces1
  flag <- branchF
  Applicative flag <$> lbranchEof flag

named :: Parser Command
named = do
  _ <- commandP ":named"
  _ <- spaces1
  flag <- branchF
  Named flag <$> nbranchEof flag

infer :: Parser Command
infer = do
  _ <- commandP ":t"
  _ <- spaces1
  flag <- branchF
  Infer flag <$> lbranchEof flag

command :: Parser Command
command = (spaces >> p) <* eof
  where
    p =
      define
        <|> echo
        <|> browse
        <|> environment
        <|> eval
        <|> substitute
        <|> alphaEquiv
        <|> alphaConvert
        <|> betas
        <|> eta1Reduce
        <|> etaReduce
        <|> liftMaximal
        <|> liftMinimal
        <|> nameless
        <|> lnameless
        <|> numeralToInt
        <|> subnameless
        <|> applicative
        <|> named
        <|> infer

instance Parseable Command where
  parse = (\res -> if null res then Left "parse error" else Right $ fst . head $ res) . _parse command
