module Part3.Tasks where

import Util (notImplementedYet)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = f (n) : (finc f (n + 1))

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = x : ff f (f (x))

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq lst = getMostFreqValue(findMaxFreq getLstWithFreq)
  where
    getSplitLst = splitAll lst
    getLstWithFreq = countAllFreq getSplitLst
    getMostFreqValue freqData = fst(freqData)

split :: Int -> [Int]
split n = if n < 10 then [n] else (n `mod` 10) : (split (n `div` 10))

splitAll :: [Int] -> [Int]
splitAll lst = concatMap split lst

countAllFreq :: [Int] -> [(Int, Int)]
countAllFreq lst = map countFreq lst
  where
    countFreq n = (n, (length (filter (== n) lst)))

findMaxFreq :: [(Int, Int)] -> (Int, Int)
findMaxFreq lst = foldr getMax first lst
  where
    first = head lst
    getMax n1 n2 = if snd(n1) > snd(n2) then n1 else n2

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq lst = nextUniq : uniq (lstWithoutNextUniq)
  where
    nextUniq = head lst
    lstWithoutNextUniq = filter (/= nextUniq) (tail lst)

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f l = merge createRes
  where
    createRes = map assignPossible l
    assignPossible n = ((f n), n)

merge :: (Eq k) =>  [(k, a)] -> [(k, [a])]
merge [] = []
merge l = (currentPossible, takeValuesByPossible) : (merge (getPartWithoutPossible otherPart))
  where
    current = head l
    otherPart = tail l
    currentPossible = fst current
    currentValue = snd current
    takeValuesByPossible = currentValue : (getByPossible otherPart)
    getPartWithPossible lst = filter (\el -> ((fst(el)) == currentPossible)) lst
    getPartWithoutPossible lst = filter (\el -> ((fst(el)) /= currentPossible)) lst
    getByPossible lst = map snd (getPartWithPossible lst)