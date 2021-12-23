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
    showsPrec = notImplementedYet
    show REmpty = "[]"
    show lst = "[" ++ rlistJoin lst ++ "]"

rlistJoin :: (Show a) => (ReverseList a) -> String
rlistJoin (REmpty :< last) = show last
rlistJoin (lst :< last) = rlistJoin lst ++ "," ++ show last

instance (Eq a) => Eq (ReverseList a) where
--    (==) REmpty REmpty = True
--    (==) REmpty (lst :< last) = False
--    (==) (lst :< last) REmpty = False
--    (==) (lst1 :< last1) (lst2 :< last2) = last1 == last2 && lst1 == lst2
--
--    (/=) REmpty REmpty = False
--    (/=) REmpty (lst :< last) = True
--    (/=) (lst :< last) REmpty = True
--    (/=) (lst1 :< last1) (lst2 :< last2) = last1 /= last2 && lst1 /= lst2
    (==) = notImplementedYet
    (/=) = notImplementedYet
instance Semigroup (ReverseList a) where
instance Monoid (ReverseList a) where
instance Functor ReverseList where
instance Applicative ReverseList where
instance Monad ReverseList where
