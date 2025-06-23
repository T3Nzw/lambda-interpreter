{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Combinators where

import NamelessTerm (NamelessTerm (..))

-- TODO implement some kind of nameless environment?
-- or maybe just put them all in the same env
-- (it already contains the named conbinators)
-- so idk?

c_I :: NamelessTerm
c_I = Abs (Var 0)

c_K :: NamelessTerm
c_K = Abs (Abs (Var 1))

c_Ω :: NamelessTerm
c_Ω = App term term
  where
    term = Abs (App (Var 0) (Var 0))

c_M :: NamelessTerm
c_M = Abs (App (Var 0) (Var 0))

c_S :: NamelessTerm
c_S = Abs $ Abs $ Abs (App (App (Var 2) (Var 0)) (App (Var 1) (Var 0)))
