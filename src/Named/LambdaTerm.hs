{-# LANGUAGE InstanceSigs #-}

module LambdaTerm where

data LambdaTerm
  = Variable String
  | Application LambdaTerm LambdaTerm
  | Abstraction String LambdaTerm
  deriving (Eq, Ord)

instance Show LambdaTerm where
  show :: LambdaTerm -> String
  show (Variable var) = var
  show (Abstraction var body) =
    "λ" ++ var ++ "." ++ show body
  show (Application t1@(Abstraction _ _) t2@(Abstraction _ _)) =
    "(" ++ show t1 ++ ") (" ++ show t2 ++ ")"
  show (Application t1@(Abstraction _ _) t2@(Application _ _)) =
    "(" ++ show t1 ++ ") (" ++ show t2 ++ ")"
  show (Application t1@(Abstraction _ _) t2) =
    "(" ++ show t1 ++ ") " ++ show t2
  show (Application t1 t2@(Abstraction _ _)) =
    show t1 ++ " (" ++ show t2 ++ ")"
  show (Application t1 t2@(Application _ _)) =
    show t1 ++ " (" ++ show t2 ++ ")"
  show (Application t1 t2) =
    show t1 ++ " " ++ show t2
