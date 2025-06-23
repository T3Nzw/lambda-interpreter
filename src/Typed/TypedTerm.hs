module TypedTerm where

import LambdaTerm (LambdaTerm (Abstraction, Application, Variable))
import STLC.Type

type Type = PType String

data TypedTerm
  = TVariable String Type
  | TApplication TypedTerm TypedTerm
  | TAbstraction String Type TypedTerm

-- TODO use type inference algorithm to find the type of the lambda term
-- and assign each subterm its corresponding type
toTyped :: LambdaTerm -> TypedTerm
toTyped = undefined

toNamed :: TypedTerm -> LambdaTerm
toNamed (TVariable x _) = Variable x
toNamed (TApplication lhs rhs) = Application (toNamed lhs) (toNamed rhs)
toNamed (TAbstraction var _ body) = Abstraction var $ toNamed body
