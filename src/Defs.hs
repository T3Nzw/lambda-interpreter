module Defs where

import Data.Map
import LambdaTerm (LambdaTerm)

type Environment = Map String (Either String LambdaTerm)

type LambdaFunction = LambdaTerm -> LambdaTerm

-- fixed value for now
limit :: Int
limit = 1000
