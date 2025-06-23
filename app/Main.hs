module Main where

import Builtin.Environment (builtinEnv)
import Control.Monad.State
import Data.Map
import Interpreter
import System.Console.Haskeline

main :: IO ()
main = evalStateT (runInputT defaultSettings repl) builtinEnv
