module Part5.Tasks where

import Util(notImplementedYet)

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f a [] = a
myFoldl f a (head:tail) = myFoldl f (f a head) tail

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f a [] = a
myFoldr f a (head:tail) = f head (myFoldr f a tail)

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле
myMap :: (a -> b) -> [a] -> [b]
myMap f lst = myFoldr toListF [] lst
  where
    toListF n l = (f n) : l

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f lst = myFoldr concatF [] lst
  where
    concatF n l = (f n) ++ l

myConcat :: [[a]] -> [a]
myConcat lst = myFoldr (++) [] lst

myReverse :: [a] -> [a]
myReverse lst = myFoldl reverse [] lst
  where
    reverse n l = l : n

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p lst = myFoldr toFiltered [] lst
  where
    toFiltered n l = if (p n) then n : l else l

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p lst = myFoldr toPartition ([], []) lst
  where
    toPartition n res = if (p n) then ((n : (fst res)), (snd res)) else ((fst res), (n : (snd res)))