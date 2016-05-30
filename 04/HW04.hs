{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.List
import Data.Monoid

newtype Poly a = P [Sum a]

getTerms :: Poly a -> [Sum a]
getTerms (P xs) = xs



-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [1]

-- Exercise 2 ----------------------------------------

chopZeros :: (Eq a, Num a) => [a] -> [a]
chopZeros [] = []
chopZeros xs = if (last xs) == 0 then chopZeros (init xs) else xs

chopPoly :: (Eq a, Num a) => Poly a -> Poly a
chopPoly = P . chopZeros . getTerms

instance (Num a, Eq a) => Eq (Poly a) where
    (P xs) == (P ys) = chopZeros xs == chopZeros ys
 
-- Exercise 3 -----------------------------------------


showTerm :: (Num a, Eq a, Show a) => Sum a -> Int -> String
showTerm (Sum coef) 0 = show coef
showTerm (Sum coef) 1 = show coef ++ "x"
showTerm (Sum coef) power
  | coef == 0 = ""
  | (abs coef) == 1 = (if coef == -1 then "-" else "") ++ "x^" ++ show power
  | otherwise = show coef ++ "x^" ++ show power

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P []) = "0"
    show (P xs) = intercalate " + " $ filter (\y -> y /= "") $ zipWith showTerm xs [0 .. ]
    --show (P (x:xs)) = 

-- Exercise 4 -----------------------------------------

monoidHead :: Monoid a => [a] -> a
monoidHead [] = mempty
monoidHead (y:_) = y

monoidTail :: Monoid a => [a] -> [a]
monoidTail [] = []
monoidTail (_:ys) = ys

zipWithPad :: (Monoid a, Monoid b) => (a -> b -> c) -> [a] -> [b] -> [c]
zipWithPad _ [] [] = []
zipWithPad f ys zs = f (monoidHead ys) (monoidHead zs) : zipWithPad f (monoidTail ys) (monoidTail zs)

zipPad :: (Monoid a, Monoid b) => [a] -> [b] -> [(a, b)]
zipPad = zipWithPad (,)

sumPad :: (Num a) => [Sum a] -> [Sum a] -> [Sum a]
sumPad = zipWithPad (+)

toSums :: (Num a) => [a] -> [Sum a]
toSums = map Sum

fromSums :: (Num a) => [Sum a] -> [a]
fromSums = map getSum

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P xs) (P ys) = P $ sumPad xs ys

-- Exercise 5 -----------------------------------------

mergeWithOffset :: (Monoid a) => ([a] -> [a] -> [a]) -> [[a]] -> [a]
mergeWithOffset _ [] = []
mergeWithOffset _ (y:[]) = y
mergeWithOffset f (y:ys) = f y (mempty:(mergeWithOffset f ys))

apListToList :: (a -> b -> c) -> [a] -> [b] -> [[c]]
apListToList f xs ys = ($ ys) . map . f <$> xs

times :: Num a => Poly a -> Poly a -> Poly a
times (P xs) (P ys) = P $ mergeWithOffset sumPad $ apListToList (*) xs ys

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate  (P xs) = P $ negate <$> xs
    fromInteger y = P [fromInteger y]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: (Num a) => Poly a -> a -> a
applyP (P xs) base = sum $ zipWith (*) (getSum <$> xs) [base^pow | pow <- [0 ..]]

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv n f = nderiv (n - 1) (deriv f)

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv (P []) = P []
    deriv (P (_:ys)) = P $ zipWith (*) ys $ fromInteger <$> [1 .. ]

