module Main where

import qualified Nameless
import qualified Repl

-- Nameless, Subterm, and Substitution contain tests
-- for some of the problems in the LCPT course, namely:

-- Substitution - 2.5
-- Nameless - 2.11 (function definition for substitute at src/Nameless/NamelessTerm.hs)
-- Subterm - 2.13 (function definition at src/Utils/NamedTerm.hs)

main :: IO ()
main = Repl.runTests >> Nameless.runTests
