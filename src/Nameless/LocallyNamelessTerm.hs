{-# LANGUAGE InstanceSigs #-}

module LocallyNamelessTerm where

import LambdaTerm (LambdaTerm (..))

-- found this so-called locally nameless representation
-- of lambda terms on reddit, and decided to add it

data LocallyNamelessTerm
  = Free String
  | Bound Int
  | Abs LocallyNamelessTerm
  | App LocallyNamelessTerm LocallyNamelessTerm
  deriving (Eq)

instance Show LocallyNamelessTerm where
  show :: LocallyNamelessTerm -> String
  show (Free x) = x
  show (Bound n) = show n
  show (Abs term) = "λ." ++ show term
  show (App lhs rhs@(App _ _)) =
    show lhs ++ " (" ++ show rhs ++ ")"
  show (App lhs@(Abs _) rhs@(Abs _)) =
    "(" ++ show lhs ++ ") (" ++ show rhs ++ ")"
  show (App lhs@(Abs _) rhs) = "(" ++ show lhs ++ ") " ++ show rhs
  show (App lhs rhs@(Abs _)) = show lhs ++ " (" ++ show rhs ++ ")"
  show (App lhs rhs) = show lhs ++ " " ++ show rhs

fromNamed :: LambdaTerm -> LocallyNamelessTerm
fromNamed = helper []
  where
    helper bv (Variable x) =
      case bound of
        Nothing -> Free x
        Just n -> Bound n
      where
        bound = lookup x bv
    helper bv (Application t1 t2) =
      App (helper bv t1) (helper bv t2)
    helper bv (Abstraction x body) =
      -- maybe use replace to optimise it :p
      Abs (helper ((x, 0) : inc) body)
      where
        inc = map (\(x, y) -> (x, y + 1)) bv

-- TODO: implement reduction
