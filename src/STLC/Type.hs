module STLC.Type where

-- stealing this from my LCPT course :D
-- it's parametric merely bc i want to make it a functor instance
data PType a = TVar a | (PType a) :=> (PType a)
  deriving (Eq, Ord)

infixr 6 :=>

instance (Show a) => Show (PType a) where
  show (TVar x) = show x
  show ((t1 :=> t2) :=> t3) =
    "(" ++ show t1 ++ " -> " ++ show t2 ++ ") -> " ++ show t3
  show (t1 :=> t2) = show t1 ++ " -> " ++ show t2

instance Functor PType where
  fmap f (TVar x) = TVar $ f x
  fmap f (lhs :=> rhs) = fmap f lhs :=> fmap f rhs
