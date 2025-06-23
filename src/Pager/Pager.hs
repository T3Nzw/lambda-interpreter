module Pager where

import Control.Exception (finally)
import System.Posix.IO
import System.Process

-- thanks to a very cool fellow on Reddit: https://www.reddit.com/r/haskell/comments/2y2f1p/pager_in_ghci_like_less/?rdt=59933

less :: IO a -> IO a
less s = do
  stdoutCopy <- dup stdOutput
  (Just ph, _, _, pid) <- createProcess (proc "less" ["-R"]) {std_in = CreatePipe}

  closeFd stdOutput

  pipeFd <- handleToFd ph

  _ <- dupTo pipeFd stdOutput

  closeFd pipeFd

  s `finally` closeFd stdOutput `finally` waitForProcess pid `finally` do
    _ <- dupTo stdoutCopy stdOutput
    closeFd stdoutCopy
