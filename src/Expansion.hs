module Expansion (expand) where

import Control.Applicative (Alternative ((<|>)))
import qualified Data.Map as M
import Debug.Trace
import Defs
import LambdaTerm (LambdaTerm (..))
import Parser
  ( Parseable (..),
    Parser (_parse),
    char,
    digit,
    eof,
    letter,
    many1,
    result,
    spaces,
    token,
    zero,
    zeroOrOne,
  )

-- unfortunately the grammar for the expansion of wildcards, etc,
-- is the very same as the lambda term parser itself :)

termVariableName :: Parser String
termVariableName = token $ many1 (letter <|> digit)

parseVariable :: Parser LambdaTerm
parseVariable = Variable <$> termVariableName

parseBinding :: Environment -> Parser LambdaTerm
parseBinding env = do
  _ <- spaces
  _ <- char '$'
  key <- parseVariable
  case M.lookup (show key) env of
    Nothing -> zero
    Just val -> case val of
      Left uneval -> either (const zero) return $ parseLT env uneval
      Right eval -> return eval

parseAbstraction :: Environment -> Parser LambdaTerm
parseAbstraction env = do
  spaces
  _ <- char '\\' <|> char 'λ'
  vars <- many1 termVariableName
  spaces
  _ <- char '.'
  body <- parseApplication env

  return $ foldr Abstraction body vars

parseParentheses :: Environment -> Parser LambdaTerm
parseParentheses env = do
  spaces
  _ <- char '('
  term <- parseApplication env
  _ <- char ')'
  return term

parseApplication :: Environment -> Parser LambdaTerm
parseApplication env = do
  terms <- many1 $ parseTerm env
  return $ foldl1 Application terms

parseTerm :: Environment -> Parser LambdaTerm
parseTerm env = spaces *> (parseAbstraction env <|> parseParentheses env <|> parseBinding env <|> parseVariable)

parseLT :: Environment -> String -> Either String LambdaTerm
parseLT env = (\res -> if null res then Left "parse error*" else Right $ fst . head $ res) . \s -> _parse (parseApplication env <* eof) s

-- | expands wildcards ($-prefixed term identifiers) into a string
expand :: String -> Environment -> String
expand initial env = either id show $ parseLT env initial
