module Lambda (Expr (..), (-$-), (-@-), evaluate) where

type Name = Int

data Expr
    = Lam Expr
    | App Expr Expr
    | Var Int

instance Show Expr where
    show (Lam f)   = "(λ." ++ show f ++ ")"
    show (App f x) = "(" ++ show f ++ " " ++ show x ++ ")"
    show (Var n)   = show n

infixr 0 -$-
infixl 1 -@-
(-@-), (-$-) :: Expr -> Expr -> Expr
(-@-) = App
(-$-) = App

-- try to get rid of recursion in Frame
--      if it can become Frame Expr Frame instead of Frame Expr [Frame]
--      then Frame can be type Frame = [Expr] and can be refactored
data Frame = Frame Expr [Frame]
reduce :: Frame -> Frame
reduce (Frame (Lam f) env) = Frame f env
reduce (Frame (Var n) env) = env !! n
reduce (Frame (App f x) env) = reduce $ Frame f' (x':env')
    where
        x'              = reduce $ Frame x env
        (Frame f' env') = reduce $ Frame f env

evaluate :: Expr -> Expr
evaluate exp = (\(Frame e env) -> Lam e) $ reduce $ Frame exp []
