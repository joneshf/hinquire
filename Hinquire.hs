{-
    This pragma is the only thing stopping me going insane with monoids.
    Not really, but the `Inquire` data should be parameterized better.
    I mean, we're restricted to one data type throughout the entire query.

    It'd be nice if we could parameterize it on a "JSON" type or something.
-}

module Main where

import Data.Bifoldable
import Data.Bifunctor
import Data.Char
import Data.Monoid
import Prelude hiding (foldr)

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
           | Not
    deriving Eq

data Inquire k v = Atom
                 | Predicate k Relation v
                 | Group (Inquire k v) GBool (Inquire k v)
                 | Wrap WBool (Inquire k v)
    deriving Eq

-- Algebra stuff

instance Monoid (Inquire k v) where
    mempty = empty
    mappend = (<&&&>)

instance Bifunctor Inquire where
    bimap _ _ Atom = Atom
    bimap f g (Predicate k r v) = Predicate (f k) r (g v)
    bimap f g (Group i1 b i2) = Group (bimap f g i1) b (bimap f g i2)
    bimap f g (Wrap b i) = Wrap b (bimap f g i)

instance Bifoldable Inquire where
    bifoldMap _ _ Atom = mempty
    bifoldMap f g (Predicate k _ v) = f k `mappend` g v
    bifoldMap f g (Group i1 _ i2) = bifoldMap f g i1 `mappend` bifoldMap f g i2
    bifoldMap f g (Wrap _ i) = bifoldMap f g i

-- Show stuff.
-- This is really ugly to me, perhaps there's a better way.

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
    show Atom = ""
    show (Predicate k r v) = show k ++ show r ++ show v
    show (Group Atom _ Atom) = ""
    show (Group Atom _ r) = show r
    show (Group l _ Atom) = show l
    show (Group p1@Predicate {} b p2@Predicate {}) =
        show p1 ++ show b ++ show p2
    show (Group p@Predicate {} b r) =
        show p ++ show b ++ "(" ++ show r ++ ")"
    show (Group l b p@Predicate {}) =
        "(" ++ show l ++ ")" ++ show b ++ show p
    show (Group l b r) =
        "(" ++ show l ++ ")" ++ show b ++ "(" ++ show r ++ ")"
    show (Wrap n i) = show n ++ "(" ++ show i ++ ")"

empty :: Inquire k v
empty = Atom

(<&&&>) :: Inquire k v -> Inquire k v -> Inquire k v
i1 <&&&> i2 = Group i1 And i2

(<|||>) :: Inquire k v -> Inquire k v -> Inquire k v
i1 <|||> i2 = Group i1 Or i2

-- Slap a question mark in front of our inquire.
generate :: (Show v, Show k) => Inquire k v -> String
generate = ('?':) . show

main :: IO ()
main = do
    let q1 = Predicate "color" Equal "red"
    let q2 = Predicate "shape" NEqual "round"
    let q3 = q1 <|||> q2
    let q4 = q1 <&&&> q3
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
    print $ second (map toUpper) q4
    -- Check the law for one case.
    print $ bimap id id notQ3 == id notQ3
    -- Do some foldable thing.
    print $ biany (const False) (=="red") q4
