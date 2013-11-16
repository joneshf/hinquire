module Main where

{-
This is like a test to see if I can make inquire's ideal typecheck properly.
-}

-- rel and bool should not be strings, they should have their own types.
data Inquire = Predicate {key :: String, val :: String, rel :: String}
    | Group {bool :: String, i1 :: Inquire, i2 :: Inquire}
    deriving (Eq)

instance Show Inquire where
    show Predicate {key=k, val=v, rel=r} = k++r++v
    show Group     {bool=b, i1=l, i2=r}  =
        "(" ++ show l ++ ")" ++ b ++ "(" ++ show r ++ ")"

main :: IO ()
main = do
    let q1 = Predicate {key="color", val="red", rel="="}
    let q2 = Predicate {key="shape", val="round", rel="!="}
    let q3 = Group {bool=";", i1=q1, i2=q2}
    print q1
    print q2
    print q3
