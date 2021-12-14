module Part2.Tasks where

import Util(notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) term1 term2 = BinaryTerm Plus term1 term2
infixl 6 |+|
(|-|) :: Term -> Term -> Term
(|-|) term1 term2 = BinaryTerm Minus term1 term2
infixl 6 |-|
(|*|) :: Term -> Term -> Term
(|*|) term1 term2 = BinaryTerm Times term1 term2
infixl 7 |*|

-- Ассоциативность слева взята, как для стандартных операторов (Приоритет: a + b * c === a + (b * c))

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression =
  case expression of
    Variable currentVarName -> if (currentVarName == varName) then replacement else expression
    BinaryTerm op lhv rhv -> BinaryTerm op (replaceVar varName replacement lhv) (replaceVar varName replacement rhv)
    _ -> expression

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate expression =
  case expression of
    BinaryTerm op lhv rhv ->
      case (op, simplify(lhv), simplify(rhv)) of
        (Plus, IntConstant const1, IntConstant const2) -> IntConstant (const1 + const2)
        (Minus, IntConstant const1, IntConstant const2) -> IntConstant (const1 - const2)
        (Times, IntConstant const1, IntConstant const2) -> IntConstant (const1 * const2)
        _ -> expression
    _ -> expression
    where
      simplify term =
        case term of
                BinaryTerm _ _ _ -> evaluate(term)
                _ -> term