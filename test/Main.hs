{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

import Control.Monad (join)
import Hedgehog (Gen)
import Hedgehog.Classes (lawsCheck, semigroupLaws)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

main :: IO ()
main = do
  True <-
    join
      $ hchoice
          [ HGen @(Eq /\ Show /\ Semigroup)
              $ Gen.string (Range.linear 0 100) Gen.unicode,
            HGen $ Gen.text (Range.linear 0 100) Gen.unicode
            ]
          (lawsCheck . semigroupLaws)
  pure ()

class (c1 a, c2 a) => (c1 /\ c2) a

instance (c1 a, c2 a) => (c1 /\ c2) a

-- lib
data HGen c = forall a. c a => HGen (Gen a)

hchoice :: [HGen c] -> (forall a. c a => Gen a -> b) -> IO b
hchoice gens k = case gens of
  [] -> error "Hedgehog.Gen.Hetero.hchoice: used with empty list"
  _ -> do
    n <- Gen.sample $ Gen.integral $ Range.constant 0 (length gens - 1)
    pure $ case gens !! n of
      HGen gen -> k gen
