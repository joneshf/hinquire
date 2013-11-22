{-# LANGUAGE FlexibleInstances #-}
{-
    This pragma is the only thing stopping me going insane with monoids.
    Not really, but the `Inquire` data should be parameterized better.
    I mean, we're restricted to one data type throughout the entire query.

    It'd be nice if we could parameterize it on a "JSON" type or something.
-}

module Main where

import Data.Monoid

-- rel and bool should not be strings, they should have their own types.
data Relation = Equal
              | NEqual
              | GThan
              | GThanE
              | LThan
              | LThanE
    deriving Eq

data GBool = And
           | Or
    deriving Eq

data WBool = NoBool
           | AndNot
           | Not
    deriving Eq

data Inquire a = Predicate { key :: a
                           , val :: a
                           , rel :: Relation
                           }
               | Group { bool :: GBool
                       , inq1 :: Inquire a
                       , inq2 :: Inquire a
                       }
               | Wrap { maybeNot :: WBool
                      , inq :: Inquire a
                      }
    deriving Eq

-- Algebra stuff

instance Functor Inquire where
    fmap f Predicate {key=k, val=v, rel=r}  =
        Predicate {key=f k, val=f v, rel=r}
    fmap f Group {bool=b, inq1=i1, inq2=i2} =
        Group {bool=b, inq1=fmap f i1, inq2=fmap f i2}
    fmap f Wrap {maybeNot=n, inq=i}         = Wrap {maybeNot=n, inq=fmap f i}

instance Monoid (Inquire String) where
    mempty = Predicate {key="", val="", rel=Equal}
    i1 `mappend` i2 = Group {bool=And, inq1=i1, inq2=i2}

-- Show stuff.

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
    show Not    = "!"

instance Show (Inquire String) where
    show Predicate {key=k, val=v, rel=r} = k ++ show r ++ v
    show Group     {bool=b, inq1=p1@Predicate {}, inq2=p2@Predicate {}} =
        show p1 ++ show b ++ show p2
    show Group     {bool=b, inq1=p@Predicate {}, inq2=r} =
        show p ++ show b ++ "(" ++ show r ++ ")"
    show Group     {bool=b, inq1=l, inq2=p@Predicate {}} =
        "(" ++ show l ++ ")" ++ show b ++ show p
    show Wrap      {maybeNot=n, inq=i} = show n ++ "(" ++ show i ++ ")"

-- Slap a question mark in front of our inquire.
generate :: Inquire String -> String
generate = ('?':) . show

main :: IO ()
main = do
    let q1 = Predicate {key="color", val="red", rel=Equal}
    let q2 = Predicate {key="shape", val="round", rel=NEqual}
    let q3 = Group {bool=Or, inq1=q1, inq2=q2}
    let q4 = Group {bool=And, inq1=q1, inq2=q3}
    let q4' = q1 <> q3
    let notQ3 = Wrap {maybeNot=Not, inq=q3}
    print $ generate q1
    print $ generate q2
    print $ generate q3
    print $ generate q4
    print $ generate q4'
    print $ "q4 == q4': " ++ show (q4 == q4')
    print $ generate notQ3
    print $ fmap tail q4
    print $ generate $ q4 <> mempty
