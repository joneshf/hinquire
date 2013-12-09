{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad
import Data.Bifunctor
import Network.Hinquire
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Function
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

prop_bifunctorId :: Inquire String String -> Bool
prop_bifunctorId i = bimap id id i == i

prop_bifunctorComp :: Inquire String String
                   -> Fun String String
                   -> Fun String String
                   -> Fun String String
                   -> Fun String String
                   -> Bool
prop_bifunctorComp i (Fun _ f1) (Fun _ g1) (Fun _ f2) (Fun _ g2) =
    bimap (f1 . g1) (f2 . g2) i == bimap f1 f2 (bimap g1 g2 i)

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
