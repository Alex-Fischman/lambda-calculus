import Lambda

main :: IO ()
main = do
    print $ evaluate $ natural 1 -@- Main.not -@- bool True

-- move these "library functions" somewhere else
bool :: Bool -> Expr
not :: Expr
bool = Lam . Lam . Var . fromEnum
not = Lam $ Var 0 -@- bool False -@- bool True

natural :: Int -> Expr
zero, incr :: Expr
zero = Lam $ Lam $ Var 0
incr = Lam $ Lam $ Lam $ Var 1 -$- Var 2 -@- Var 1 -@- Var 0
natural n
    | n < 0  = zero
    | n == 0 = zero
    | n > 0  = incr -$- natural $ n-1
