module Main where

{-
This is like a test to see if I can make inquire's ideal typecheck properly.
-}

-- rel and bool should not be strings, they should have their own types.
data Relation = Equal | NEqual | GThan | GThanE | LThan | LThanE

data Boolean = And | Or

data Inquire = Predicate {key :: String, val :: String, rel :: Relation}
    | Group {bool :: Boolean, i1 :: Inquire, i2 :: Inquire}

instance Show Relation where
    show Equal  = "="
    show NEqual = "!="
    show GThan  = ">"
    show GThanE = ">="
    show LThan  = "<"
    show LThanE = "<="

instance Show Boolean where
    show And = "&"
    show Or  = ";"

instance Show Inquire where
    show Predicate {key=k, val=v, rel=r} = k ++ show r ++ v
    show Group     {bool=b, i1=l, i2=r}  =
        "(" ++ show l ++ ")" ++ show b ++ "(" ++ show r ++ ")"

main :: IO ()
main = do
    let q1 = Predicate {key="color", val="red", rel=Equal}
    let q2 = Predicate {key="shape", val="round", rel=NEqual}
    let q3 = Group {bool=Or, i1=q1, i2=q2}
    print q1
    print q2
    print q3
