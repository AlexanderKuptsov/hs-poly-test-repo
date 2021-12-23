module Part4.Tasks where

import Util(notImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist = foldl (:<) REmpty

-- Реализуйте все представленные ниже классы (см. тесты)
instance (Show a) => Show (ReverseList a) where
-- Минимальное определение: show или showsPrec
  show REmpty = "[]"
  show lst = "[" ++ rlistJoin lst ++ "]"
    where
      rlistJoin rLst = case rLst of
        (REmpty :< last) -> show last
        (init :< last) -> rlistJoin init ++ "," ++ show last

instance (Eq a) => Eq (ReverseList a) where
-- Минимальное определение: == или /=
  (==) REmpty REmpty = True
  (==) REmpty (init :< last) = False
  (==) (init :< last) REmpty = False
  (==) (init1 :< last1) (init2 :< last2) = last1 == last2 && init1 == init2

instance Semigroup (ReverseList a) where
  (<>) REmpty REmpty = REmpty
  (<>) lst REmpty = lst
  (<>) REmpty lst = lst
  (<>) lst1 (init2 :< last2) = lst1 <> init2 :< last2

instance Monoid (ReverseList a) where
  mempty = REmpty
  mappend = (<>)
  mconcat = foldr mappend mempty

instance Functor ReverseList where
  fmap f REmpty = REmpty
  fmap f (init :< last) = fmap f init :< f last

instance Applicative ReverseList where
  pure a = REmpty :< a
  (<*>) lst REmpty = REmpty
  (<*>) REmpty lst = REmpty
  (<*>) (init1 :< last1) lst @ (init2 :< last2) = (init1 <*> lst) <> mappedPart
    where
      mappedPart = (fmap last1 init2) :< last1 last2

instance Monad ReverseList where
  return a = (REmpty :< a)
  (>>=) REmpty f = REmpty
  (>>=) (init :< last) f = (init >>= f) <> (f last)
