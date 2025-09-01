module Spawn where

import Distribution.Compat.Prelude (ExitCode (ExitFailure, ExitSuccess))
import System.Process

execProcess :: String -> IO String
execProcess cmd = do
  -- non-exhaustive, fix
  let (name : args) = words cmd
  (exitcode, out, err) <- readProcessWithExitCode name args []
  pure $
    if exitcode == ExitSuccess
      -- potentially also show stderr output
      then out
      else
        "Process failed with exit code: "
          ++ let (ExitFailure ec) = exitcode
              in show ec
                  ++ "\n"
