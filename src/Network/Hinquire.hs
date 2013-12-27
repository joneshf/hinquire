module Network.Hinquire where

import Prelude hiding (foldr)

import Control.Applicative (Alternative (..), Applicative, pure, (<*>), (<$>), liftA3)
import Control.Monad
import Data.Biapplicative
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Char
import Data.Foldable
import Data.Monoid
import Data.Traversable

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
    mempty = Atom
    mappend = (<&&&>)

instance Functor (Inquire k) where
    fmap _ Atom = Atom
    fmap f (Predicate k r v) = Predicate k r (f v)
    fmap f (Group i1 b i2) = Group (fmap f i1) b (fmap f i2)
    fmap f (Wrap b i) = Wrap b (fmap f i)

instance Monoid k => Applicative (Inquire k) where
    pure = Predicate mempty Equal

    Atom <*> _ = Atom
    _ <*> Atom = Atom
    (Predicate _ _ f) <*> (Predicate k r v) = Predicate k r (f v)
    p@Predicate {} <*> (Group i1 b i2) = Group (p <*> i1) b (p <*> i2)
    p@Predicate {} <*> (Wrap b i) = Wrap b (p <*> i)
    (Group i1 b i2) <*> i3 = Group (i1 <*> i3) b (i2 <*> i3)
    (Wrap b i1) <*> i2 = Wrap b (i1 <*> i2)

instance Monoid k => Alternative (Inquire k) where
    empty = Atom

    Atom <|> i = i
    i    <|> _ = i

instance Foldable (Inquire k) where
    foldr _ z Atom = z
    foldr f z (Predicate _ _ v) = f v z
    foldr f z (Group i1 _ i2) = foldr f (foldr f z i2) i1
    foldr f z (Wrap _ i) = foldr f z i

instance Traversable (Inquire k) where
    traverse _ Atom = pure Atom
    traverse f (Predicate k r v) = Predicate <$> pure k <*> pure r <*> f v
    traverse f (Group i1 b i2) =
        Group <$> traverse f i1 <*> pure b <*> traverse f i1
    traverse f (Wrap b i) = Wrap <$> pure b <*> traverse f i

instance Monoid k => Monad (Inquire k) where
    return = Predicate mempty Equal

    Atom >>= _ = Atom
    -- This seems wrong,
    -- we've forgotten everything about our Predicate except the value
    (Predicate _ _ v) >>= f = f v
    (Group i1 b i2) >>= f = Group (i1 >>= f) b (i2 >>= f)
    (Wrap b i) >>= f = Wrap b (i >>= f)

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
    bipure k = Predicate k Equal

    Atom <<*>> _ = Atom
    _ <<*>> Atom = Atom
    (Predicate f _ g) <<*>> (Predicate k2 r v2) = Predicate (f k2) r (g v2)
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

class Dyad d where
    bireturn :: a -> b -> d a b
    (>>==) :: d a b -> (a -> b -> d e f) -> d e f

instance Dyad Inquire where
    bireturn k = Predicate k Equal

    Atom >>== _ = Atom
    (Predicate k _ v) >>== f = f k v
    (Group i1 b i2) >>== f = Group (i1 >>== f) b (i2 >>== f)
    (Wrap b i) >>== f = Wrap b (i >>== f)

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

-- | Conjoin two Inquires.
(<&&&>) :: Inquire k v -> Inquire k v -> Inquire k v
i1 <&&&> i2 = Group i1 And i2

-- | Disjoin two Inquires.
(<|||>) :: Inquire k v -> Inquire k v -> Inquire k v
i1 <|||> i2 = Group i1 Or i2

-- | Slap a question mark in front of our inquire.
generate :: (Show v, Show k) => Inquire k v -> String
generate = ('?':) . show
