{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad
import Data.Biapplicative
import Data.Bifunctor
import Network.Hinquire
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Function
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

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
