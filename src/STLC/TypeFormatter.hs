module TypeFormatter where

import Data.Set (Set)
import Inference (Type)

uniqueTypes :: Type -> Set
uniqueTypes (Type n) = 
