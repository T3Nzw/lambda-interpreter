module Nameless.Parser where

-- i've barely tested it, i hope it works :D

import Control.Applicative (Alternative ((<|>)))
import NamelessTerm
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

termVarName :: Parser Int
termVarName = read <$> token (many1 digit)

-- a variable is any sequence of alphanumerical characters
parseVar :: Parser NamelessTerm
parseVar = Var <$> termVarName

parseAbs :: Parser NamelessTerm
parseAbs = do
  spaces
  _ <- char '\\' <|> char 'λ'
  _ <- char '.'
  body <- parseApp

  return $ Abs body

parseParentheses :: Parser NamelessTerm
parseParentheses = do
  spaces -- it was missing :D and the parser was NOT parsing.
  _ <- char '('
  term <- parseApp
  _ <- char ')'
  return term

parseApp :: Parser NamelessTerm
parseApp = do
  terms <- many1 parseTerm
  return $ foldl1 App terms

parseTerm :: Parser NamelessTerm
parseTerm = parseAbs <|> parseVar <|> parseParentheses

parseLT :: String -> Either String NamelessTerm
parseLT = (\res -> if null res then Left "parse error" else Right $ fst . head $ res) . \s -> _parse (parseApp <* eof) s

instance Parseable NamelessTerm where
  parse = parseLT
