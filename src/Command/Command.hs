module Command where

import CommandParser (Command)
import CommandResult
import Control.Monad.State
import Defs (Environment)
import System.Console.Haskeline (InputT, getInputLine, outputStrLn)

type REPL a = InputT (StateT Environment IO) a

handleCommand :: Command -> REPL ()
handleCommand cmd = do
  env <- lift get
  helper $ interpretCommand env cmd
  where
    helper :: CommandResult -> REPL ()
    helper NoOp = return ()
    helper (Output output) = outputStrLn output
    helper (Error err) = outputStrLn $ "Error occured: " ++ err
    helper (UpdateEnv f) = do
      (msg, env') <- lift $ gets f
      lift $ modify $ const env'
      case msg of
        [] -> return ()
        _ -> outputStrLn msg
    helper (StepThrough prompt terms) =
      -- potentially allow for termination when a term is fully reduced
      case terms of
        [] -> outputStrLn $ prompt ++ "input exhausted"
        (t : ts) ->
          helper $
            Compound
              [ Output (show t),
                RequireInput $ \input -> StepThrough prompt ts
              ]
    helper (RequireInput cont) = undefined
    helper (Compound cmds) = mapM_ helper cmds
