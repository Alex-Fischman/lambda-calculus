module BLC where

import Data.List.Split

main :: IO ()
main = do
    print . evaluate . decode $ [O, O, O, O, O, I, I, I, O, I, O]
    print . decode . encode $ Lam $ Lam $ Var 1 -$- Var 0

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

data E = E Expr [E]
reduce :: E -> E
reduce (E (Lam f)   env) = E f env
reduce (E (Var n)   env) = env !! n
reduce (E (App f x) env) = reduce $ E f' (x':env')
    where
        x'          = reduce $ E x env
        (E f' env') = reduce $ E f env

evaluate :: Expr -> Expr
evaluate exp = (\(E e env) -> Lam e) $ reduce $ E exp []

data Bit = I | O deriving (Show, Read, Eq, Ord, Enum, Bounded)

parse :: [Bit] -> (Expr, [Bit])
parse (O:O:xs) = (Lam x, ys)
    where (x, ys) = parse xs
parse (O:I:xs) = (App x y, zs)
    where (x, ys) = parse xs; (y, zs) = parse ys
parse (I:xs)   = (Var index, drop (index+1) xs)
    where index = length $ takeWhile (==I) $ xs

decode :: [Bit] -> Expr
decode xs = let (x, ys) = parse xs in x

encode :: Expr -> [Bit]
encode (Lam f)   = O:O:(encode f)
encode (App f x) = O:I:((encode f)++(encode x))
encode (Var n)   = (iterate (\x -> I:x) [I] !! n)++[O]
