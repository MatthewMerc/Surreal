{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
import qualified Data.Set as Set

import Data.List

-- Axiom 1
data Surreal = Surreal {left :: Set.Set Surreal, right :: Set.Set Surreal}

(<|>) :: Maybe Surreal -> Maybe Surreal -> Surreal
(<|>) Nothing Nothing = Surreal empty empty
(<|>) Nothing (Just r) = Surreal empty (Set.singleton r)
(<|>) (Just l) Nothing = Surreal (Set.singleton l) empty
(<|>) (Just l) (Just r) = Surreal (Set.singleton l) (Set.singleton r)


-- "Day 0"
empty :: Set.Set a
empty = Set.empty

surZero :: Surreal
surZero = Nothing <|> Nothing

-- "Day 1"
surOne :: Surreal
surOne = Surreal (Set.singleton surZero) empty

surNOne :: Surreal
surNOne = -surOne

-- Just figuring out function currying here mainly
show' ::(Surreal -> Set.Set Surreal) -> Surreal -> [String]
show' x = Set.toList . Set.map show . x


-- Axiom 2. One number is less than or equal to another number
-- if and only if no member of the first number’s left set is greater than or equal to the second number,
-- and no member of the second number’s right set is less than or equal to the first number [1].

instance Ord Surreal where
  (<=) :: Surreal -> Surreal -> Bool
  x <= y =  not ( True `Set.member` Set.map (>= y) (left x))
      && not ( True `Set.member` Set.map (<= x) (right y))

-- Definition of 'likeness' see Theorem 1.5
instance Eq Surreal where
  (==) :: Surreal -> Surreal -> Bool
  x == y = x <= y && y <= x

--Defining addition on a set

add :: Set.Set Surreal -> Surreal -> Set.Set Surreal
add x y = Set.map (+y) x



instance Num Surreal where
  -- Definition 2.1
  negate :: Surreal -> Surreal
  negate x
    | x == surZero =  x
    | otherwise = Surreal (Set.map negate (right x)) (Set.map negate (left x))
  (+) :: Surreal -> Surreal -> Surreal
  -- Definition 2.2. x + y = {XL + y, x + YL|XR + y, x + YR}
  (+) x y = Surreal  (Set.union (left y `add` x) (left x `add` y )) (Set.union (right y `add` x) (right x `add` y))
  (-) :: Surreal -> Surreal -> Surreal
  -- Definition 2.3. x − y = x + (−y)
  (-) x y = x + negate y
  -- Definition 2.4.
  -- x·y = {XL·y+x·YL−XL·YL,XR·y+x·YR−XR·YR|XL·y+x·YR−XL·YR,XR·y+x·YL−XR·YL}.
  (*) :: Surreal -> Surreal -> Surreal
  (*) x y = let
        sOp op s s' = Set.unions $ Set.map (\si -> Set.map (op si) s') s
        mult = sOp (*)
        addS = sOp (+)
        subS = sOp subtract
        multl s n = Set.map (* n) s
        a = (left x `multl` y) `addS` (left y `multl` x) `subS` (left x `mult` left y)
        b = (right x `multl` y) `addS` (right y `multl` x) `subS` (right x `mult` right y)
        c = (left x `multl` y) `addS` (right y `multl` x) `subS` (left x `mult` right y)
        d = (right x `multl` y) `addS` (left y `multl` x) `subS` (right x `mult` left y)
    in
        Surreal (Set.union a b) (Set.union c d)
  abs :: Surreal -> Surreal
  abs s
    | s < surZero = negate s
    | otherwise = s
  signum :: Surreal -> Surreal
  signum s
    | s < surZero = surNOne
    | s > surZero = surOne
    | otherwise = surZero
  fromInteger :: Integer -> Surreal
  fromInteger n
    | n > 0 = surOne + fromInteger (n-1)
    | n < 0 = surNOne + fromInteger (n+1)
    | otherwise = surZero


-- Definition 1.1. We say that a number x is simpler than a number y if x was created before y.
simpler :: Surreal -> Surreal -> Bool
simpler x y = x `elem` (left y `Set.union` right y)

-- Theorem 2.2. Suppose that the different numbers at the end of n days are x1 <x2 <···<xm.
-- Then the only new numbers that will be created on the (n + 1)st day are 
-- {|x1}, {x1|x2}, . . . , {xm−1|xm}, {xm|}.

-- TODO: Generate as Binary tree
nextDay :: [Surreal] -> [Surreal]
nextDay xs =  go xs []
  where
    go :: [Surreal] -> [Surreal] -> [Surreal]
    go [] _ = [surZero]
    go [a] [] = [Nothing <|> Just a ,a,Just a <|> Nothing]
    go (a:as) [] = go as [ Nothing  <|> Just a, a, Just a <|> Just (head as)]
    go [a] acc = acc ++ [Just a <|> Nothing, a]
    go (a:as) acc = go as acc ++ [a, Just a <|> Just (head as)]


type Day = Int

genSurreal :: Day -> Set.Set Surreal
genSurreal d = Set.fromList $ iterate nextDay [surZero] !! d

instance Show Surreal where
  show s = "{"  ++ intercalate "," (show' left s) ++ "|" ++ intercalate "," (show' right s) ++ "}"

main :: IO ()
main = do
  print ((surOne + surOne) * (surOne + surOne) )