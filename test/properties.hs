{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.Biapplicative
import Data.Bifunctor
import Data.Bitraversable
import Network.Hinquire
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Function
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.Foldable (Foldable(foldMap))
import Data.Traversable (Traversable(traverse))

newtype Identity a = Identity { runIdentity :: a } deriving Eq

instance Functor Identity where
    fmap f m = Identity (f (runIdentity m))

instance Foldable Identity where
    foldMap f (Identity x) = f x

instance Traversable Identity where
    traverse f (Identity x) = Identity <$> f x

instance Applicative Identity where
    pure a = Identity a
    Identity f <*> Identity x = Identity (f x)

instance Monad Identity where
    return a = Identity a
    m >>= k  = k (runIdentity m)

instance MonadFix Identity where
    mfix f = Identity (fix (runIdentity . f))

newtype Compose f g a = Compose { getCompose :: f (g a) } deriving Eq

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose x) = Compose (fmap (fmap f) x)

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap f (Compose t) = foldMap (foldMap f) t

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    traverse f (Compose t) = Compose <$> traverse (traverse f) t

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure x = Compose (pure (pure x))
    Compose f <*> Compose x = Compose ((<*>) <$> f <*> x)

instance (Alternative f, Applicative g) => Alternative (Compose f g) where
    empty = Compose Control.Applicative.empty
    Compose x <|> Compose y = Compose (x <|> y)

prop_functor_id :: Inquire String String -> Bool
prop_functor_id i = fmap id i == i

prop_functor_comp :: Inquire String String
                  -> Fun String String
                  -> Fun String String
                  -> Bool
prop_functor_comp i (Fun _ f) (Fun _ g) =
    fmap (f . g) i == fmap f (fmap g i)

prop_bifunctor_id :: Inquire String String -> Bool
prop_bifunctor_id i = bimap id id i == i

prop_bifunctor_comp :: Inquire String String
                    -> Fun String String
                    -> Fun String String
                    -> Fun String String
                    -> Fun String String
                    -> Bool
prop_bifunctor_comp i (Fun _ f1) (Fun _ g1) (Fun _ f2) (Fun _ g2) =
    bimap (f1 . g1) (f2 . g2) i == bimap f1 f2 (bimap g1 g2 i)

prop_applicative_id :: Inquire String String -> Bool
prop_applicative_id i = (pure id <*> i) == i

prop_applicative_composition :: Inquire String String
                             -> Fun String String
                             -> Fun String String
                             -> Bool
prop_applicative_composition w (Fun _ u1) (Fun _ v1) =
    (pure (.) <*> u <*> v <*> w) == (u <*> (v <*> w))
    where u = pure u1
          v = pure v1

prop_applicative_homomorphism :: Fun String String
                              -> String
                              -> Bool
prop_applicative_homomorphism (Fun _ f) v =
    (pure f <*> pure v :: Inquire String String) == pure (f v)

prop_applicative_interchange :: Fun String String
                             -> String
                             -> Bool
prop_applicative_interchange (Fun _ f) v =
    (u <*> pure v :: Inquire String String) == (pure ($ v) <*> u)
    where u = pure f

prop_biapplicative_id :: Inquire String String -> Bool
prop_biapplicative_id i = (bipure id id <<*>> i) == i

prop_biapplicative_composition :: Inquire String String
                               -> Fun String String
                               -> Fun String String
                               -> Fun String String
                               -> Fun String String
                               -> Bool
prop_biapplicative_composition w (Fun _ u1) (Fun _ u2) (Fun _ v1) (Fun _ v2) =
    (bipure (.) (.) <<*>> u <<*>> v <<*>> w) == (u <<*>> (v <<*>> w))
    where u = bipure u1 u2
          v = bipure v1 v2

prop_biapplicative_homomorphism :: Fun String String
                                -> Fun String String
                                -> String
                                -> String
                                -> Bool
prop_biapplicative_homomorphism (Fun _ f) (Fun _ g) k v =
    (bipure f g <<*>> bipure k v :: Inquire String String) == bipure (f k) (g v)

prop_biapplicative_interchange :: Fun String String
                               -> Fun String String
                               -> String
                               -> String
                               -> Bool
prop_biapplicative_interchange (Fun _ f) (Fun _ g) k v =
    (u <<*>> bipure k v :: Inquire String String) == (bipure ($ k) ($ v) <<*>> u)
    where u = bipure f g

prop_bitraversable_id :: Inquire String String -> Bool
prop_bitraversable_id i =
    bitraverse Identity Identity i == Identity i

--prop_bitraversable_comp :: Inquire String String
--                        -> Fun String [String]
--                        -> Fun String [String]
--                        -> Fun String [String]
--                        -> Fun String [String]
--                        -> Bool
--prop_bitraversable_comp i (Fun _ f1) (Fun _ f2) (Fun _ g1) (Fun _ g2) =
--    bitraverse (Compose . fmap g1 . f1) (Compose . fmap g2 . f2) i ==
--        (Compose . fmap (bitraverse g1 g2) . (bitraverse f1 f2)) i

instance (Arbitrary k, Arbitrary v) => Arbitrary (Inquire k v) where
    arbitrary = sized inquire
        where
            inquire 0 = return Atom
            inquire 1 = liftM3 Predicate arbitrary rel arbitrary
            inquire n = oneof [ return Atom
                              , liftM3 Predicate arbitrary rel arbitrary
                              , liftM3 Group inquire' gBool inquire'
                              , liftM2 Wrap wBool inquire'
                              ]
                where
                    inquire' = inquire (n `div` 2)
            rel = elements [Equal, NEqual, GThan, GThanE, LThan, LThanE]
            gBool = elements [And, Or]
            wBool = elements [NoBool, Not]

    shrink Atom              = []
    shrink (Predicate k r v) = do
        k' <- shrink k
        v' <- shrink v
        return $ Predicate k' r v'
    shrink (Group i1 b i2)   = do
        i1' <- shrink i1
        i2' <- shrink i2
        return $ Group i1' b i2'
    shrink (Wrap b i)        = do
        i' <- shrink i
        return $ Wrap b i'

main = $defaultMainGenerator
