-- not too shabby, seems to parse some things :D

module LambdaParser where

import Control.Applicative (Alternative ((<|>)))
import LambdaTerm (LambdaTerm (..))
import Parser
  ( Parseable (..),
    Parser (_parse),
    char,
    digit,
    eof,
    lower,
    many1,
    spaces,
    token,
  )

termVariableName :: Parser String
termVariableName = token $ many1 (lower <|> digit)

-- a variable is any sequence of alphanumerical characters
parseVariable :: Parser LambdaTerm
parseVariable = Variable <$> termVariableName

parseAbstraction :: Parser LambdaTerm
parseAbstraction = do
  spaces
  _ <- char '\\' <|> char 'λ'
  vars <- many1 termVariableName
  spaces
  _ <- char '.'
  -- parseTerm, here for debugging purposes
  body <- parseApplication

  -- foldr over all vars with nv body to form lambda term
  return $ foldr Abstraction body vars

parseParentheses :: Parser LambdaTerm
parseParentheses = do
  spaces -- it was missing :D and the parser was NOT parsing.
  _ <- char '('
  term <- parseApplication
  _ <- char ')'
  return term

parseApplication :: Parser LambdaTerm
parseApplication = do
  terms <- many1 parseTerm
  return $ foldl1 Application terms

parseTerm :: Parser LambdaTerm
parseTerm = parseAbstraction <|> parseVariable <|> parseParentheses

parseLT :: String -> Either String LambdaTerm
parseLT = (\res -> if null res then Left "parse error" else Right $ fst . head $ res) . \s -> _parse (parseApplication <* eof) s

instance Parseable LambdaTerm where
  parse = parseLT
