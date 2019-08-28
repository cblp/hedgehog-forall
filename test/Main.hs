import Hedgehog.Classes (lawsCheck, semigroupLaws)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

main :: IO ()
main = do
  True <-
    lawsCheck $ semigroupLaws $ Gen.string (Range.linear 0 100) Gen.unicode
  True <- lawsCheck $ semigroupLaws $ Gen.text (Range.linear 0 100) Gen.unicode
  pure ()
