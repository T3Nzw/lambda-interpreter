module Utils.StepThrough where

import Control.Monad (void)
import Control.Monad.State (StateT)
import qualified Data.Map as M
import Distribution.Utils.String (trim)
import LambdaTerm (LambdaTerm)
import Reduction (LambdaFunction)
import System.Console.Haskeline

type Environment = M.Map String (Either String LambdaTerm)

type REPL a = InputT (StateT Environment IO) a

stages :: LambdaFunction -> LambdaTerm -> [LambdaTerm]
stages = iterate

-- TODO: fix :D, HELP, why doesn't haskell allow for do-notations in patterns????
stepthrough :: String -> [LambdaTerm] -> REPL ()
stepthrough = helper 0
  where
    helper :: Int -> String -> [LambdaTerm] -> REPL ()
    helper n prompt xs = do
      -- not my fault haskell cannot encode infinitude in its type system
      let (x : ys@(y : _)) = xs
      outputStrLn $ show n ++ ". " ++ show x
      if x == y
        then void $ outputStrLn (prompt ++ "-reduction terminated.")
        else outputStrLn $ prompt ++ "> continue? (<enter>/q)"
      input <- getInputLine ""
      case trim <$> input of
        Nothing -> return ()
        Just "" -> helper (n + 1) prompt ys
        Just "q" -> return ()
        Just ":q" -> return ()
        _ -> outputStrLn $ prompt ++ "> error: invalid command"
