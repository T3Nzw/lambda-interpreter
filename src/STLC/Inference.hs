{-# LANGUAGE DerivingStrategies #-}

-- algorithm taken from Salinger's notes on lambda calculus :)
-- not properly implemented still (a total mess i must say)

module STLC.Inference where

import Control.Monad.State
import qualified Data.Map as M
import LambdaTerm
import STLC.Type
import System.IO.Unsafe (unsafePerformIO)
import Utils.NamedTerm

type Type = PType String

type Context = M.Map LambdaTerm Type

type TypeSubstitution = Type -> Type

-- replace 1 with 2 in 3
replaceInType :: Type -> Type -> Type -> Type
replaceInType old new ty@(TVar _) =
  if ty == old then new else ty
replaceInType old new (ty1 :=> ty2) =
  replaceInType old new ty1 :=> replaceInType old new ty2

-- thanks chatgpt, i was bashing my head against the wall
-- update: potentially does nothing useful
flatten :: [Type] -> TypeSubstitution
flatten [] = id
flatten (t : ts) = snd $ foldl step (t, id) ts
  where
    step :: (Type, TypeSubstitution) -> Type -> (Type, TypeSubstitution)
    step (acc, subst) t' =
      let t1 = subst acc
          t2 = subst t'
          s = mgu t1 t2
       in (s t1, s . subst)

-- i forgot what this was supposed to do?
typesub :: Type -> TypeSubstitution -> Type
typesub ty sigma = sigma ty

-- ty1 occurs in ty2 or vice versa
occursCheck :: Type -> Type -> Bool
occursCheck (TVar x) (TVar y) = x == y
occursCheck ty1@(TVar _) (ty21 :=> ty22) =
  occursCheck ty1 ty21 || occursCheck ty1 ty22
occursCheck (ty11 :=> ty12) ty2@(TVar _) =
  occursCheck ty2 ty11 || occursCheck ty2 ty12
occursCheck (ty11 :=> ty12) (ty21 :=> ty22) =
  occursCheck ty11 ty21 || occursCheck ty11 ty22 || occursCheck ty12 ty21 || occursCheck ty12 ty22

-- most general unifier for two types
mgu :: Type -> Type -> TypeSubstitution
mgu ty1@(TVar _) ty2
  | ty1 == ty2 = id
  | ty1 /= ty2 && not (occursCheck ty1 ty2) = replaceInType ty1 ty2
  | otherwise = error "type substitution failed"
mgu ty1 ty2@(TVar _)
  | ty1 == ty2 = id
  | ty1 /= ty2 && not (occursCheck ty2 ty1) = replaceInType ty2 ty1
  | otherwise = error "type substitution failed"
mgu (ty11 :=> ty12) (ty21 :=> ty22) =
  mgu ty11 ty21 . mgu ty12 ty22

-- most general unifier for a list of types, idk how to use this
-- UPDATE: perhaps this can be used instead of flatten? just
-- replace every single type inferred for a variable with the same variable
mgu' :: [Type] -> Type -> TypeSubstitution
mgu' [] _ = id
mgu' (x : xs) new = mgu x new . mgu' xs new

{-
\xy.xy
ctx = {x:t0, y:t1}
xy:t2
x:t3 -> t2 where t3 is fresh
y: t3

infer x: lookup x in ctx and create substitution using mgu
infer y: lookup y in ctx and create substitution rules using mgu

xy -> unify generated mgu's and apply accordingly

i think it's better to apply mgu alg on a list of constraints

THE MGU IS APPLIED DIRECTLY TO THE GENERATED TYPE OF THE TERM. peace
-}

-- i literally had to write out the process
-- of deducing the type of this function BY HAND
-- in order for it to work properly in typeinfer
-- HOLY SHIT IT ACTUALLY WORKED WTFFFFFFFFFFFFFFFFFFFFF
-- TYPE DRIVEN DEVELOPMENT, BABY
-- update: this shit does nothing
mmap :: IO TypeSubstitution -> IO [Type] -> IO [Type]
mmap f l = do
  g <- f
  (g <$>) <$> l

-- fuck this. fix it so that it doesn't use genSym (as that might be causing unwanted behaviour due to side effects?)
-- UPDATE: it was probably what was causing problems with inferring the type of (\x.x) : "x1" -> "x12" (???)
-- will just use unsafePerformIO for now, might eventually make it purer and use the reader monad :)
-- but i can't be bothered rn, especially since chances are high the algorithm isn't even correctly implemented,
-- and i am already losing my mind debugging it
-- UPDATE 2: the side effects in genSym WERE the problem :)
-- UPDATE 3: my logic is ALSO problematic :) turns out \x y.x and \x y.y have the same type, aye :)
-- the only term the type of which my alg infers correctly is the identity, LOL, that's encouraging
-- 1 > 0 or something idk
-- TODO debug this because i am pretty sure i messed up the algorithm somewhere
-- UPDATE 4: ditch this genSym thing because unwrapping the value
-- ended up causing just as many problems, namely
-- (\x y.x) : x3 -> x3 -> x3 (definitely an inhabited type btw)
{-
typeinfer :: LambdaTerm -> Context -> (TypeSubstitution, Type)
typeinfer t@(Variable x) ctx =
  case varty of
    Nothing -> error $ "unbound variable " ++ x
    Just types -> (sub, typesub (head types) sub)
  where
    varty = M.lookup t ctx
    sub = case varty of
      Nothing -> error $ "unbound variable " ++ x
      Just typeasm -> mgu' typeasm (head typeasm)
typeinfer (Application lhs rhs) ctx =
  (comp, finalapp)
  where
    appty = TVar $ unsafePerformIO $ genSym "t"
    freshvar = TVar $ unsafePerformIO $ genSym "a"
    lconstraint = freshvar :=> appty
    lasm = case M.lookup lhs ctx of
      Nothing -> [lconstraint] -- wtf?
      Just asm -> lconstraint : asm
    (lsub, ltype) = typeinfer lhs $ M.insert lhs lasm ctx
    lctx = M.map (map lsub) ctx
    lfreshvar = typesub freshvar lsub
    (rsub, rtype) = typeinfer rhs $ M.insert rhs [lfreshvar] lctx
    comp = rsub . lsub
    finalapp = typesub appty comp
typeinfer (Abstraction var body) ctx =
  (comp, finalvar :=> bodytype)
  where
    varty = TVar $ unsafePerformIO $ genSym "v"
    bodyty = TVar $ unsafePerformIO $ genSym "b"
    freshvar = TVar $ unsafePerformIO $ genSym "x"
    varsub = mgu bodyty $ varty :=> freshvar
    tmpctx = M.insert (Variable var) [varty] ctx
    newctx = M.map (map varsub) tmpctx
    freshvarsub = typesub freshvar varsub
    (bodysub, bodytype) = typeinfer body $ M.insert body [freshvarsub] newctx
    comp = bodysub . varsub
    finalvar = typesub freshvarsub comp
-}
-- implement the whole thing anew,
-- it's too messy to debug...
type Identifier = String

-- genSym ruined me, the state monad it is (<3)
type InferState = State [Identifier] (Either String (TypeSubstitution, Type))

typevars = map (('t' :) . show) [0 ..]

-- typeinfer' :: LambdaTerm -> Context -> InferState
-- typeinfer' var@(Variable x) ctx = do
--   case varty of
--     Nothing -> pure $ Left $ "unbound variable " ++ x
--     Just ty -> let unifyingType = head ty in pure $ Right (mgu' ty unifyingType, unifyingType)
--   where
--     varty = M.lookup var ctx
-- typeinfer' (Application lhs rhs) ctx = do
--   freshvar <- gets head
--   modify tail
--   let ltype = 0
--   pure $ Left "error"
--
-- -- bit of a monad wizard (update: the type isn't wrapped in an IO monad anymore, but it looked cooler before, i promise)
-- -- la fonction de la sub does nothingggg
-- infer :: LambdaTerm -> Type
-- infer term = submap ty
--   where
--     (sub, ty) = typeinfer term M.empty
--     submap ty@(TVar _) = typesub ty sub
--     submap (lhs :=> rhs) = submap lhs :=> submap rhs
