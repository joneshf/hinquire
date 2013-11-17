module Main where

{-
This is like a test to see if I can make inquire's ideal typecheck properly.
-}

-- rel and bool should not be strings, they should have their own types.
data Relation = Equal | NEqual | GThan | GThanE | LThan | LThanE

data GBool = And | Or

data WBool = NoBool | AndNot | Not

data Inquire = Predicate {key :: String, val :: String, rel :: Relation}
    | Group {bool :: GBool, inq1 :: Inquire, inq2 :: Inquire}
    | Wrap {maybeNot :: WBool, inq :: Inquire}

instance Show Relation where
    show Equal  = "="
    show NEqual = "!="
    show GThan  = ">"
    show GThanE = ">="
    show LThan  = "<"
    show LThanE = "<="

instance Show GBool where
    show And = "&"
    show Or  = ";"

instance Show WBool where
    show NoBool = ""
    -- I'm not sure if `AndNot` is actually needed,
    -- but I ran into it with the ls version.
    show AndNot = "&!"
    show Not    = "!"

instance Show Inquire where
    show Predicate {key=k, val=v, rel=r}    = k ++ show r ++ v
    show Group     {bool=b, inq1=l, inq2=r} =
        "(" ++ show l ++ ")" ++ show b ++ "(" ++ show r ++ ")"
    show Wrap      {maybeNot=n, inq=i}      = show n ++ "(" ++ show i ++ ")"

main :: IO ()
main = do
    let q1 = Predicate {key="color", val="red", rel=Equal}
    let q2 = Predicate {key="shape", val="round", rel=NEqual}
    let q3 = Group {bool=Or, inq1=q1, inq2=q2}
    let notQ3 = Wrap {maybeNot=Not, inq=q3}
    print q1
    print q2
    print q3
    print notQ3
