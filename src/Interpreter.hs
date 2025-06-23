-- perhaps map a function to a parser succeeding?
-- how do i even do that?

-- TODO: fix visualisation of terms that contain digits?????????

module Interpreter where

import Command (REPL, handleCommand)
import CommandHelp (generalHelp)
import Control.Monad.State (evalState, get, liftIO)
import Distribution.Utils.String (trim)
import Pager (less)
import Parser (Parseable (parse))
import System.Console.Haskeline (getInputLine, outputStr, outputStrLn)
import System.Info (os)
import System.Process (callCommand)

errorMsg :: String
errorMsg = "\ESC[31mERROR: invalid command.\n\ESC[33mType \":help\" to see more information on how to use <insert program name here>."

repl :: REPL ()
repl = do
  outputStr "\ESC[33mλ>\ESC[0m "
  input <- getInputLine ""
  case trim <$> input of
    Nothing -> return ()
    Just "" -> repl
    Just ":exit" -> return ()
    Just ":q" -> return ()
    Just ":clear" -> (liftIO . callCommand $ if os == "mingw32" then "cls" else "clear") >> repl
    Just ":help" -> case os of
      -- portable? who knows
      "mingw32" -> outputStrLn generalHelp >> repl
      _ -> liftIO (less $ putStr generalHelp) >> repl
    Just s -> do
      let cmd = parse s
      -- outputStrLn $ show cmd
      case cmd of
        Left _ -> outputStrLn errorMsg
        Right cmdType -> handleCommand cmdType
      repl
