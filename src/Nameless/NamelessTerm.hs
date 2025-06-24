{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

-- TODO: carefully trace fromNamed's implementation since i might have some very slight bugs tehre

module NamelessTerm where

import qualified Data.Map as M
import LambdaTerm (LambdaTerm (..))

-- the following code did not see a bright future
-- but i'm leaving it here nonetheless for future reference
-- (actual code starts from line 53 onwards)

-- leveraging phantom types for type safety ;)
-- and also GADTs! purposefully tagging the indices,
-- since i can't look up a variable in the context
-- unless i were to assume its type (bound or free) beforehand,
-- which would in turn make the code a lot sloppier
-- lowkey useless:)
type Identifier = String

type TIndex c = Int

data Bound

data Free

-- a bunch of useless shit :)

-- this was extremely useless and further made my head hurt
-- bc good luck with removing free variables from the context
-- if both free and bound variables reside in the same fucking context
data TaggedIndex where
  BoundVariable :: TIndex Bound -> TaggedIndex
  FreeVariable :: TIndex Free -> TaggedIndex

getIndex :: TaggedIndex -> Int
getIndex (BoundVariable n) = n
getIndex (FreeVariable n) = n

-- could've also been Int -> Int,
-- no particular reason for using rank 2 (ad hoc) polymorphism,
-- but i learned about it today, might just as well
-- shove it in places it does not belong to
tagMap :: (forall a. (Num a) => a -> a) -> TaggedIndex -> TaggedIndex
tagMap f (BoundVariable n) = BoundVariable $ f n
tagMap f (FreeVariable n) = FreeVariable $ f n

type TContext = M.Map Identifier TaggedIndex

-- actually useful stuff

type Index = Int

type Context a = M.Map Identifier Index

data NamelessTerm
  = Var Int
  | App NamelessTerm NamelessTerm
  | Abs NamelessTerm
  deriving (Eq, Ord)

instance Show NamelessTerm where
  show :: NamelessTerm -> String
  show (Var n) = show n
  show (Abs body) = "λ." ++ show body
  show (App lhs@(Abs _) rhs@(Abs _)) =
    "(" ++ show lhs ++ ") (" ++ show rhs ++ ")"
  show (App lhs@(Abs _) rhs@(App _ _)) =
    "(" ++ show lhs ++ ") (" ++ show rhs ++ ")"
  show (App lhs@(Abs _) rhs) =
    "(" ++ show lhs ++ ") " ++ show rhs
  show (App lhs rhs@(Abs _)) =
    show lhs ++ " (" ++ show rhs ++ ")"
  show (App lhs rhs@(App _ _)) =
    show lhs ++ " (" ++ show rhs ++ ")"
  show (App lhs rhs) = show lhs ++ " " ++ show rhs

-- i genuinely wanted to use type tagging so badly here
{-
fromNamed :: LambdaTerm -> NamelessTerm
fromNamed = snd . helper 0 M.empty
  where
    helper :: Int -> TContext -> LambdaTerm -> (TContext, NamelessTerm)
    helper _ ctx (Variable x) =
      (M.insert x varIndex ctx, Var $ getIndex varIndex)
      where
        varIndex = case M.lookup x ctx of
          Nothing -> FreeVariable $ M.size ctx
          Just index -> index
    helper n ctx (Application lhs rhs) =
      (rctx, App lterm rterm)
      where
        (lctx, lterm) = helper n ctx lhs
        (rctx, rterm) = helper n lctx rhs
    helper n ctx (Abstraction var body) =
      (ctx, Abs resbody)
      where
        (_, resbody) = helper (n + 1) (M.insert var (BoundVariable 0) $ M.map (tagMap (+ 1)) ctx) body
-}

-- a state monad would look great here
fromNamed :: LambdaTerm -> NamelessTerm
fromNamed = (\(_, _, x) -> x) . helper 0 M.empty M.empty
  where
    helper :: Int -> Context Bound -> Context Free -> LambdaTerm -> (Context Bound, Context Free, NamelessTerm)
    helper n bv fv (Variable x) =
      case M.lookup x bv of
        Nothing -> case M.lookup x fv of
          Nothing -> let idx = M.size fv + n in (bv, M.insert x idx fv, Var idx)
          Just idx -> (bv, fv, Var idx)
        Just idx -> (bv, fv, Var idx)
    helper n bv fv (Application lhs rhs) =
      (rbv, rfv, App lterm rterm)
      where
        (_, lfv, lterm) = helper n bv fv lhs
        (rbv, rfv, rterm) = helper n bv lfv rhs
    helper n bv fv (Abstraction var body) =
      (bv, fv, Abs body1)
      where
        (_, _, body1) = helper (n + 1) (M.insert var 0 $ M.map (+ 1) bv) M.empty body

shift :: Int -> Int -> NamelessTerm -> NamelessTerm
shift c d (Var i)
  | 0 <= i && i < c = Var i
  | otherwise = Var $ i + d
shift c d (App lhs rhs) = App (shift c d lhs) (shift c d rhs)
shift c d (Abs body) = Abs $ shift (c + 1) d body

substitute :: Int -> NamelessTerm -> NamelessTerm -> NamelessTerm
substitute old new (Var i)
  | old == i = new
  | otherwise = Var i
substitute old new (App lhs rhs) = App (substitute old new lhs) (substitute old new rhs)
substitute old new (Abs body) = Abs $ substitute (old + 1) (shift 0 1 new) body

largestIndex :: NamelessTerm -> Int
largestIndex (Var i) = i
largestIndex (App lhs rhs) = max (largestIndex lhs) (largestIndex rhs)
largestIndex (Abs body) = largestIndex body

toNamed :: NamelessTerm -> LambdaTerm
toNamed term = helper (-1) term
  where
    ctx :: [String]
    ctx = ('x' :) . show <$> [0 .. largestIndex term]

    helper :: Index -> NamelessTerm -> LambdaTerm
    helper k (Var i)
      | k - i >= 0 = Variable $ ctx !! (k - i)
      | otherwise = Variable $ ctx !! (k + 1)
    helper _ (App lhs@(Abs _) rhs@(Abs _)) = Application (helper (-1) lhs) (helper (-1) rhs)
    helper k (App lhs@(Abs _) rhs) = Application (helper (-1) lhs) (helper k rhs)
    helper k (App lhs rhs@(Abs _)) = Application (helper k lhs) (helper (-1) rhs)
    helper k (App lhs rhs) = Application (helper k lhs) (helper k rhs)
    helper k (Abs body) = Abstraction (ctx !! (k + 1)) $ helper (k + 1) body
