import qualified Data.Set as Set

-- Axiom 1
data Surreal = Surreal {left :: Set.Set Surreal, right :: Set.Set Surreal}

-- "Day 0"
surEmpty = Set.empty

surZero = Surreal surEmpty surEmpty

-- "Day 1"
surOne = Surreal (Set.singleton surZero) surEmpty

surNOne = Surreal surEmpty (Set.singleton surZero)

-- Axiom 2. One number is less than or equal to another number
-- if and only if no member of the first number’s left set is greater than or equal to the second number,
-- and no member of the second number’s right set is less than or equal to the first number [1].

instance Ord Surreal where
  x <= y = not $ Set.member True (Set.map (< y) (left x)) || Set.member True (Set.map (< x) (right y))

instance Eq Surreal where
  x == y = (x <= y) && (y <= x)

-- Definition 1.1. We say that a number x is simpler than a number y if x was created before y.
simpler :: Surreal -> Surreal -> Bool
simpler x y = x `elem` (left y `Set.union` right y)

main :: IO ()
main = do 
    print (surZero > surOne)
    print (surZero `simpler` surOne)