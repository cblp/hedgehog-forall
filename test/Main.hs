{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

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
          [ HGen @SemigroupLaws $ Gen.string (Range.linear 0 100) Gen.unicode,
            HGen $ Gen.text (Range.linear 0 100) Gen.unicode
            ]
          (lawsCheck . semigroupLaws)
  pure ()

class (Eq a, Semigroup a, Show a) => SemigroupLaws a

instance (Eq a, Semigroup a, Show a) => SemigroupLaws a

-- lib
data HGen c = forall a. c a => HGen (Gen a)

hchoice :: [HGen c] -> (forall a. c a => Gen a -> b) -> IO b
hchoice gens k = case gens of
  [] -> error "Hedgehog.Gen.Hetero.hchoice: used with empty list"
  _ -> do
    n <- Gen.sample $ Gen.integral $ Range.constant 0 (length gens - 1)
    pure $ case gens !! n of
      HGen gen -> k gen
