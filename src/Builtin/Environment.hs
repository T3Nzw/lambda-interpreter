module Builtin.Environment where

import Church.Numerals
import Church.Pair
import Data.Bifunctor
import qualified Data.Map as M
import Defs
import LambdaTerm (LambdaTerm)

builtinEnv :: Environment
builtinEnv = M.fromList $ map (second Right) $ allPairs ++ allNumerals

showEnv :: M.Map String (Either String LambdaTerm) -> String
showEnv = helper . M.toList
  where
    helper [] = ""
    helper [x] = showKVP x
    helper (x : xs) = showKVP x ++ "\n" ++ helper xs

    showKVP (key, value) =
      "\""
        ++ key
        ++ "\" : "
        ++ case value of
          Left s -> "\"" ++ s ++ "\" (not evaluated)"
          Right term -> show term
