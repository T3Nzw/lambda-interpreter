module Utils.Substitution where

-- TODO i need a smarter way to rename variables..........

-- i forgot what most of these functions do
-- and looking back on them they seem QUESTIONABLE

import Data.Char
import qualified Data.Foldable as Foldable
import qualified Data.Ord as Ord
import qualified Data.Set as S
import LambdaTerm (LambdaTerm (..))
import Utils.NamedTerm

type BoundVariables = [String]

fixpoint :: (Eq a) => (a -> a) -> a -> Bool
fixpoint f x = f x == x

repeatWhile :: (Eq a) => (a -> a -> Bool) -> (a -> a) -> a -> a
repeatWhile p f x = if p x (f x) then x else repeatWhile p f (f x)

variables :: LambdaTerm -> S.Set String
variables = helper S.empty
  where
    helper :: S.Set String -> LambdaTerm -> S.Set String
    helper set (Variable x) = S.insert x set
    helper set (Application lhs rhs) =
      S.union (helper set lhs) (helper set rhs)
    helper set (Abstraction var body) =
      helper (S.insert var set) body

substitute :: LambdaTerm -> LambdaTerm -> LambdaTerm -> LambdaTerm
substitute old new t@(Variable _) =
  if old == t then new else t
substitute old new t@(Abstraction var body) =
  if Variable var == old || var `elem` extractFreeVars new
    then t
    else Abstraction var $ substitute old new body
substitute old new (Application lhs rhs) =
  Application (substitute old new lhs) (substitute old new rhs)

replaceInTerm :: BoundVariables -> LambdaTerm -> LambdaTerm -> LambdaTerm -> LambdaTerm
replaceInTerm bound old new t@(Variable x) =
  if old == t && x `notElem` bound then new else t
replaceInTerm bound old new (Abstraction var body) =
  Abstraction var $ replaceInTerm (var : bound) old new body
replaceInTerm bound old new (Application lhs rhs) =
  Application (replaceInTerm bound old new lhs) (replaceInTerm bound old new rhs)

alphaConvert' :: String -> String -> LambdaTerm -> LambdaTerm
alphaConvert' = helper S.empty
  where
    helper :: S.Set String -> String -> String -> LambdaTerm -> LambdaTerm
    helper bv old new (Variable x)
      | x `S.member` bv && old == x = Variable new
      | otherwise = Variable x
    helper bv old new (Application lhs rhs) =
      Application (helper bv old new lhs) (helper bv old new rhs)
    helper bv old new (Abstraction var body) =
      Abstraction newvar $ helper (var `S.insert` bv) old new body
      where
        newvar = if old == var then new else var

-- its entire purpose is to rename all variables that will be captured by substitution
-- might (definitely does) contain slight bugs;
-- it takes the same arguments (in the same order) as substitute:
-- (\x.y x)[y ~> x] without renaming would become \x.x x, instead of
-- (\x.y x)[y ~> x]  ==  \x'.x x' (alpha-equivalent!)
-- i think the arguments are swapped :D
-- old is actually new, and new is old
-- UPDATE: i think i've got the general idea down
-- but i somehow managed to mix up old and new and idk what does what anymore
renameCaptured :: String -> String -> LambdaTerm -> LambdaTerm
renameCaptured old new term = snd $ helper S.empty old new term
  where
    vars :: S.Set String
    vars = variables term

    -- extract all unique variable names (vars), fetch the longest name,
    -- and generate a fresh variable name by appending 0 to it :)
    -- at the very least it's guaranteed not to cause any name clashes
    -- or futher variable captures
    freshvar = (++ "0") $ Foldable.maximumBy (Ord.comparing length) $ S.toList vars

    helper :: S.Set String -> String -> String -> LambdaTerm -> (Bool, LambdaTerm)
    helper bv old new (Variable x)
      | old == x && x `S.member` bv = (True, Variable freshvar)
      | otherwise = (False, Variable x)
    helper bv old new (Application lhs@(Abstraction _ _) rhs@(Abstraction _ _)) =
      (False, Application lhs1 rhs1)
      where
        (_, lhs1) = helper bv old new lhs
        (_, rhs1) = helper bv old new rhs
    helper bv old new (Application lhs@(Abstraction _ _) rhs) =
      (flag2, Application lhs1 rhs1)
      where
        (_, lhs1) = helper bv old new lhs
        (flag2, rhs1) = helper bv old new rhs
    helper bv old new (Application lhs rhs@(Abstraction _ _)) =
      (flag1, Application lhs1 rhs1)
      where
        (flag1, lhs1) = helper bv old new lhs
        (_, rhs1) = helper bv old new rhs
    helper bv old new (Application lhs rhs) =
      (flag1 || flag2, Application lhs1 rhs1)
      where
        (flag1, lhs1) = helper bv old new lhs
        (flag2, rhs1) = helper bv old new rhs
    helper bv old new (Abstraction var body)
      | flag || old == var = (flag, Abstraction freshvar resterm)
      | otherwise = (flag, Abstraction var resterm)
      where
        (flag, resterm) = helper (var `S.insert` bv) old new body

rename :: String -> String -> LambdaTerm -> LambdaTerm
rename old new (Variable x)
  | old == x = Variable new
  | otherwise = Variable x
rename old new (Application lhs rhs) =
  Application (rename old new lhs) (rename old new rhs)
rename old new (Abstraction var body)
  | old == var = Abstraction new resterm
  | otherwise = Abstraction var resterm
  where
    resterm = rename old new body

-- generates the next variable name based on a list of
-- previously used variable names so as to avoid name clashes
-- generating new variable names suddenly isn't all that trivial..............................
-- TODO: fix generating of fresh variables so that they DO NOT take up the entire screen eventually
-- UPDATE: the idea i came up with (definitely not the most optimal or clean one, but an idea nonetheless)
-- is to collect all numeric suffixes in variable names (e.g. 13 in x13, BUT NOT 13 in x13y)
-- and, using some cool(?) algebraic manipulation (that i have yet to figure out),
-- find a "minimal" (or so to speak, an interval that has the minimal possible values)
-- range of indices that does not overlap with the one collected from traversing the term
-- for the first time, and then rename all variables in the term with the newly formed variable names,
-- obtained by taking the remaining part after extracting the numeric suffix
-- (again, that would be x in 13, and x13y in x13y) and appending the new suffix at the end :)
nextName :: String -> [String] -> String
nextName prev used = head [new | n <- [0 ..] :: [Int], let new = takeWhile isAlpha prev ++ show n, show n `notElem` used]

-- renames every single bound variable
alphaConvert :: LambdaTerm -> LambdaTerm
alphaConvert = fst . helper []
  where
    helper used t@(Variable _) = (t, used)
    helper used (Application t1 t2) =
      (Application term1 term2, used2)
      where
        (term1, used1) = helper used t1
        (term2, used2) = helper used1 t2
    helper used (Abstraction var body) =
      (Abstraction newName term1, used1)
      where
        newName = nextName var used
        replaced = rename var newName body
        (term1, used1) = helper (dropWhile isAlpha newName : used) replaced

currySubstitution :: String -> LambdaTerm -> LambdaTerm -> LambdaTerm
currySubstitution = helper
  where
    freshvar :: [String] -> String
    freshvar vars = (++ "0") $ Foldable.maximumBy (Ord.comparing length) vars

    helper :: String -> LambdaTerm -> LambdaTerm -> LambdaTerm
    helper old new (Variable x)
      | old == x = new
      | otherwise = Variable x
    helper old new (Application lhs rhs) =
      Application (helper old new lhs) (helper old new rhs)
    helper old new t@(Abstraction var body)
      | old == var = t
      | not (old `isFreeIn` body) || not (var `isFreeIn` new) = Abstraction var $ helper old new body
      | otherwise = Abstraction fresh $ helper old new (helper var (Variable fresh) body)
      where
        fresh = freshvar $ extractFreeVars body ++ extractFreeVars new
