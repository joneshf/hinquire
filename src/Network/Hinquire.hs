module Network.Hinquire where

import Control.Applicative (Applicative, pure, (<*>), (<$>))
import Data.Biapplicative
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Char
import Data.Monoid

-- | The relation between a key and value "time=now" or "cat!=dog"
data Relation = Equal
              | NEqual
              | GThan
              | GThanE
              | LThan
              | LThanE
    deriving Eq

-- | The boolean operation between a group of Inquires
data GBool = And
           | Or
    deriving Eq

-- | This is an optional negation wrapping an Inquire.
data WBool = NoBool
           | Not
    deriving Eq

-- | The meat of our package. This encapsulates our query logic.
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
    bifoldr _ _ z Atom = z
    bifoldr f g z (Predicate k _ v) = f k $ g v z
    bifoldr f g z (Group i1 _ i2) = bifoldr f g (bifoldr f g z i2) i1
    bifoldr f g z (Wrap _ i) = bifoldr f g z i

instance Biapplicative Inquire where
    bipure k v = Predicate k Equal v

    (Predicate _ _ _) <<*>> Atom = Atom
    (Predicate k1 _ v1) <<*>> (Predicate k2 r v2) = Predicate (k1 k2) r (v1 v2)
    p@Predicate {} <<*>> (Group i1 b i2) = Group (p <<*>> i1) b (p <<*>> i2)
    p@Predicate {} <<*>> (Wrap b i) = Wrap b (p <<*>> i)
    (Group i1 b i2) <<*>> i3 = Group (i1 <<*>> i3) b (i1 <<*>> i3)
    (Wrap b i1) <<*>> i2 = Wrap b (i1 <<*>> i2)

instance Bitraversable Inquire where
    bitraverse _ _ Atom = pure Atom
    bitraverse f g (Predicate k r v) = Predicate <$> f k <*> pure r <*> g v
    bitraverse f g (Group i1 b i2) =
        Group <$> bitraverse f g i1 <*> pure b <*> bitraverse f g i2
    bitraverse f g (Wrap b i) = Wrap <$> pure b <*> bitraverse f g i

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

-- This is really ugly to me, perhaps there's a better way.

instance (Show k, Show v) => Show (Inquire k v) where
    show Atom = ""
    show (Predicate k r v) = show k ++ show r ++ show v
    show (Group Atom _ Atom) = ""
    show (Group Atom _ r) = show r
    show (Group l    _ Atom) = show l
    show (Group l@Predicate {} b r@Predicate {}) = show l ++ show b ++ show r
    show (Group l@Predicate {} b r) = show l ++ show b ++ "(" ++ show r ++ ")"
    show (Group l b r@Predicate {}) = "(" ++ show l ++ ")" ++ show b ++ show r
    show (Group l b r) = "(" ++ show l ++ ")" ++ show b ++ "(" ++ show r ++ ")"
    show (Wrap n i) = show n ++ "(" ++ show i ++ ")"

empty :: Inquire k v
empty = Atom

-- | Conjoin two Inquires.
(<&&&>) :: Inquire k v -> Inquire k v -> Inquire k v
i1 <&&&> i2 = Group i1 And i2

-- | Disjoin two Inquires.
(<|||>) :: Inquire k v -> Inquire k v -> Inquire k v
i1 <|||> i2 = Group i1 Or i2

-- | Slap a question mark in front of our inquire.
generate :: (Show v, Show k) => Inquire k v -> String
generate = ('?':) . show
