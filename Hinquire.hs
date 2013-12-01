{-# LANGUAGE DeriveFoldable, DeriveFunctor, FlexibleInstances #-}
{-
    This pragma is the only thing stopping me going insane with monoids.
    Not really, but the `Inquire` data should be parameterized better.
    I mean, we're restricted to one data type throughout the entire query.

    It'd be nice if we could parameterize it on a "JSON" type or something.
-}

module Main where

import Data.Foldable
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

data Inquire k v = Atom
                 | Predicate k Relation v
                 | Group (Inquire k v) GBool (Inquire k v)
                 | Wrap WBool (Inquire k v)
    deriving (Eq, Foldable, Functor)

-- Algebra stuff

instance Monoid (Inquire k v) where
    mempty = empty
    mappend = inqAnd

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

instance (Show k, Show v) => Show (Inquire k v) where
    show (Predicate k r v) = show k ++ show r ++ show v
    show (Group p1@Predicate {} b p2@Predicate {}) =
        show p1 ++ show b ++ show p2
    show (Group p@Predicate {} b r) =
        show p ++ show b ++ "(" ++ show r ++ ")"
    show (Group l b p@Predicate {}) =
        "(" ++ show l ++ ")" ++ show b ++ show p
    show (Wrap n i) = show n ++ "(" ++ show i ++ ")"

empty :: Inquire k v
empty = Atom

inqAnd :: Inquire k v -> Inquire k v -> Inquire k v
inqAnd i1 = Group i1 And

-- Slap a question mark in front of our inquire.
generate :: (Show v, Show k) => Inquire k v -> String
generate = ('?':) . show

main :: IO ()
main = do
    let q1 = Predicate "color" Equal "red"
    let q2 = Predicate "shape" NEqual "round"
    let q3 = Group q1 Or q2
    let q4 = Group q1 And q3
    let q4' = q1 <> q3
    let notQ3 = Wrap Not q3
    print $ generate q1
    print $ generate q2
    print $ generate q3
    print $ generate q4
    print $ generate q4'
    print $ "q4 == q4': " ++ show (q4 == q4')
    print $ generate notQ3
    print $ generate $ q4 <> mempty
    -- Do some functor thing.
    print $ fmap tail q4
    -- Do some foldable thing.
    print $ "red" `Data.Foldable.elem` q4
