module Lambda (Expr (..), (-$-), (-.-), evaluate) where

data Expr
    = Lam Expr
    | App Expr Expr
    | Var Int

instance Show Expr where
    show (Lam f)   = "(λ." ++ show f ++ ")"
    show (App f x) = "(" ++ show f ++ " " ++ show x ++ ")"
    show (Var n)   = show n

infixr 0 -$-
infixl 9 -.-
(-.-), (-$-) :: Expr -> Expr -> Expr
(-.-) = App
(-$-) = App

-- try to get rid of recursion in E
--      if it can become E Expr E instead of E Expr [E]
--      then data E can be type E = [Expr] and can be refactored
data E = E Expr [E]
reduce :: E -> E
reduce (E (Lam f) env) = E f env
reduce (E (Var n) env) = env !! n
reduce (E (App f x) env) = reduce $ E f' (x':env')
    where
        x'              = reduce $ E x env
        (E f' env') = reduce $ E f env

evaluate :: Expr -> Expr
evaluate exp = (\(E e env) -> Lam e) $ reduce $ E exp []
