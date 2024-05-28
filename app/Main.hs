{-# LANGUAGE InstanceSigs #-}
import qualified Data.Set as Set

import Data.List

-- Axiom 1
data Surreal = Surreal {left :: Set.Set Surreal, right :: Set.Set Surreal}

-- "Day 0"
empty :: Set.Set a
empty = Set.empty

surZero :: Surreal
surZero = Surreal empty empty

-- "Day 1"
surOne :: Surreal
surOne = Surreal (Set.singleton surZero) empty

surNOne :: Surreal
surNOne = Surreal empty (Set.singleton surZero)



show' ::(Surreal -> Set.Set Surreal) -> Surreal -> [String]
show' x = Set.toList . Set.map show . x

r :: Surreal -> [String]
r = show' right
l :: Surreal -> [String]
l = show' left


instance Show Surreal where
  show s = "{"  ++ intercalate "," (l s) ++ "|" ++ intercalate "," (r s) ++ "}"

-- Axiom 2. One number is less than or equal to another number
-- if and only if no member of the first number’s left set is greater than or equal to the second number,
-- and no member of the second number’s right set is less than or equal to the first number [1].

instance Ord Surreal where
  (<=) :: Surreal -> Surreal -> Bool
  x <= y =  not ( True `Set.member` Set.map (>= y) (left x))
      && not ( True `Set.member` Set.map (<= x) (right y))

instance Eq Surreal where
  x == y = x <= y && y <= x

-- Definition 1.1. We say that a number x is simpler than a number y if x was created before y.
simpler :: Surreal -> Surreal -> Bool
simpler x y = x `elem` (left y `Set.union` right y)

main :: IO ()
main = do
    print (surZero > surOne)
    print (surZero `simpler` surOne)
    print $ Surreal (Set.singleton surNOne) (Set.fromList [  surOne, surZero])