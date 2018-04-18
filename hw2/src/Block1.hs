module Block1
-- (eval, bin2, Expr, ArithmeticError)
  where


data Expr = Const Int
    | Add {a :: Expr, b :: Expr}
    | Sub {a :: Expr, b :: Expr}
    | Mul {a :: Expr, b :: Expr}
    | Div {a :: Expr, b :: Expr}
    | Pow {a :: Expr, b :: Expr}

data ArithmeticError = DivisionByZero | NegativePower deriving (Show, Read, Eq)


eval :: Expr -> Either ArithmeticError Int
eval expr = case expr of
    (Const c) -> Right c
    _         -> eval (a expr) >>=
                    \a1 -> eval (b expr) >>=
                        \b1 -> case expr of
                           (Const c) -> Right c
                           (Add _ _) -> Right $ a1 + b1
                           (Sub _ _) -> Right $ a1 - b1
                           (Mul _ _) -> Right $ a1 * b1
                           (Div _ _) ->
                               if b1 == 0
                                   then Left DivisionByZero
                                   else  Right $ a1 `div` b1
                           (Pow _ _) ->
                               if b1 < 0
                                   then Left NegativePower
                                   else Right $ a1 ^ b1



data Exception = IllegalArgumentException deriving (Show, Read, Eq)

bin :: Int ->  Either Exception [[Int]]
bin x =
    if x <  0 then Left IllegalArgumentException
    else let t x1 = if x1 == 0 then [[]]
                    else  t (x1 - 1) >>= \res ->  [0 : res, 1 : res]
    in Right $ t x


bin2 :: Int -> [[Int]]
bin2 x = let t x1 = if x1 == 0 then [[]]
                    else  t (x1 - 1) >>= \res ->  [0 : res, 1 : res]
        in  t x


