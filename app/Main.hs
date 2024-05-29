{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
import qualified Data.Set as Set

import Data.List

-- Axiom 1
data Surreal = Surreal {left :: Set.Set Surreal, right :: Set.Set Surreal}

-- (<|>) :: Surreal -> Surreal -> Surreal

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
surNOne = Surreal empty (Set.singleton surZero)


-- Just figuring out function currying here mainly
show' ::(Surreal -> Set.Set Surreal) -> Surreal -> [String]
show' x = Set.toList . Set.map show . x

instance Show Surreal where
  show s = "{"  ++ intercalate "," (show' left s) ++ "|" ++ intercalate "," (show' right s) ++ "}"

-- Axiom 2. One number is less than or equal to another number
-- if and only if no member of the first number’s left set is greater than or equal to the second number,
-- and no member of the second number’s right set is less than or equal to the first number [1].

instance Ord Surreal where
  (<=) :: Surreal -> Surreal -> Bool
  x <= y =  not ( True `Set.member` Set.map (>= y) (left x))
      && not ( True `Set.member` Set.map (<= x) (right y))

instance Eq Surreal where
  (==) :: Surreal -> Surreal -> Bool
  x == y = x <= y && y <= x



instance Num Surreal where
  (+) :: Surreal -> Surreal -> Surreal
  -- Definition 2.2. x + y = {XL + y, x + YL|XR + y, x + YR}
  (+) x y = Surreal  (Set.union (Set.map (+x) (left y)) (Set.map (+y) (left x) )) (Set.union (Set.map (+x) (right y)) (Set.map (+y) (right x) ))
  (-) :: Surreal -> Surreal -> Surreal
  -- Definition 2.3. x − y = x + (−y)
  (-) x y = x + negate y
  -- TODO
  (*) :: Surreal -> Surreal -> Surreal
  (*) = (+)
  -- Definition 2.1
  negate :: Surreal -> Surreal
  negate s
    | s == surZero =  s
    | otherwise = Surreal (Set.map negate (right s)) (Set.map negate (left s))
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
    | n > 0 = surOne + fromInteger(n-1)
    | n < 0 = surNOne + fromInteger(n+1)
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

main :: IO ()
main = do
    print (  surZero - surNOne)
    -- print $ Surreal (Set.singleton surNOne) (Set.fromList [  surOne, surZero])