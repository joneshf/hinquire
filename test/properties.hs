{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Bifunctor
import Network.Hinquire
import Test.QuickCheck
import Test.QuickCheck.All
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

prop_bifunctorId i1 = bimap id id i1 == id i1
    where _ = i1 :: Inquire String String

prop_bifunctorBad i1 = i1 == Atom
    where _ = i1 :: Inquire String String

instance (Arbitrary k, Arbitrary v) => Arbitrary (Inquire k v) where
    arbitrary = frequency [ (1, return Atom)
                          , (2, arbPred)
                          ]
        where
            arbPred = do
                key <- arbitrary
                rel <- elements [Equal, NEqual, GThan, GThanE, LThan, LThanE]
                val <- arbitrary
                return $ Predicate key rel val

    shrink Atom              = []
    shrink (Predicate k r v) = do
        k' <- shrink k
        v' <- shrink v
        return $ Predicate k' r v'

main = $defaultMainGenerator

