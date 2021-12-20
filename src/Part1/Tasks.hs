module Part1.Tasks where

import Util(notImplementedYet)
import Data.List

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x = mySin' x 125

mySin' x m = mySinTaylor (normalize x) 0 m

mySinTaylor :: Double -> Double -> Double -> Double
mySinTaylor x k m
  | k > m = 0
  | otherwise = (((-1)**(k)) * (x**(2 * k + 1)) / fact (2 * k + 1)) + (mySinTaylor x (k + 1) m)

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x = myCos' x 125

myCos' x m = myCosTaylor (normalize x) 0 m

myCosTaylor :: Double -> Double -> Double -> Double
myCosTaylor x k m
  | k > m = 0
  | otherwise = (((-1)**(k)) * (x**(2 * k)) / fact (2 * k)) + (myCosTaylor x (k + 1) m)

fact i = if i <= 1 then 1 else i * fact (i - 1)

normalize :: Double -> Double
normalize x = x - 2 * pi * (fromIntegral $ floor (x / (2 * pi)))

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD a b
  | x == 0 = y
  | y == 0 = x
  | otherwise = myGCD (maxN `mod` minN) minN
  where
     x = abs(a)
     y = abs(b)
     minN = (min x y)
     maxN = (max x y)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isFebruaryCorrect :: Integer -> Integer -> Bool
isFebruaryCorrect day year
  | day < 29 = True
  | (day == 29  && (year `mod` 400 == 0 || (year `mod` 4 == 0 && year `mod` 100 /= 0))) = True
  | otherwise = False

isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year
  | year < minValidYear = False
  | month < 1 || month > 12 = False
  | day < 1 = False
  | month `elem` bigMonths && day <=31 = True
  | month `elem` commonMonths && day<=30 = True
  | month == 2 = isFebruaryCorrect day year
  | otherwise = False
  where
    minValidYear = 1970
    bigMonths = [1, 3, 5, 7, 8, 10, 12]
    commonMonths = [4, 6, 9, 7, 11]

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow x 0 = 1
myPow x 1 = x
myPow x n = x * myPow x (n - 1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x
  | x <= 1 = False
  | otherwise = myPrime x 2

myPrime :: Integer -> Integer  -> Bool
myPrime x del
  | 2 * del >= x = True
  | x `mod` del == 0 = False
  | otherwise = myPrime x (del + 1)

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = 0.5 * abs(gaussLeft - gaussRight)
  where
    gaussLeft = gaussSeq points fst snd
    gaussRight = gaussSeq points snd fst

gaussSeq points f1 f2 = f1(points !! (n - 1)) * f2(points !! 0) + (gaussSeq' 0)
  where
    n = length points
    gaussSeq' i
      | i == (n - 1) = 0
      | otherwise = f1(points !! i) * f2(points !! (i + 1)) + (gaussSeq' (i + 1))

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он остроугольный
--  1, если он прямоугольный
--  2, если он тупоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c
  | maxSide >= otherSide1 + otherSide2 || maxSideSqr <= abs(otherSide1 - otherSide2) = -1
  | maxSideSqr == otherSidesSqrSum = 1
  | maxSideSqr < otherSidesSqrSum = 0
  | maxSideSqr > otherSidesSqrSum = 2
  | otherwise = -1
    where
      sidesByLength = sort [a, b, c]
      maxSide = sidesByLength !! 2
      otherSide1 = sidesByLength !! 0
      otherSide2 = sidesByLength !! 1
      maxSideSqr = maxSide**2
      otherSidesSqrSum = otherSide1**2 + otherSide2**2