{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Data.Array
import Control.Monad.Fix

direct :: Integer -> Integer
direct = fix step

step :: (Integer -> Integer) -> Integer -> Integer
step _ 1 = 0
step f x | x <= 0    = error $ "Non-positive arg: " ++ show x
         | even x    = 1 + (f (x `div` 2))
         | otherwise = 1 + (f (3*x +1))
         

table :: Array Int Integer
table = listArray (1, 10000000) $ 0 : (map go [2..])
  where go x = let index = if even x then x `div` 2 else 3*x + 1
               in 1 + memo index

memo :: Integer -> Integer
memo x | inRange (bounds table) (fromIntegral x) = table ! (fromIntegral x)
       | otherwise = step memo x
       
main = print $ maximum $ map (\x -> (memo x, x)) [1..10000000]
