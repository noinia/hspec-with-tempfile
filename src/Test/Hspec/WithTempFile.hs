module Test.Hspec.WithTempFile
  ( golden



  , spec
  ) where

import Test.Hspec
import Test.Hspec.WithTempFile.Golden

--------------------------------------------------------------------------------

fib   :: Int -> Int
fib n = fibs !! n
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

myTest = GoldenTest defaultGolden (fib 5)

spec :: Spec
spec = describe "example test" $ do
         golden "simple Text test" defaultGolden (fib 5)
         goldenTest "simple Text test" $ myTest

-- | Combinator to run a golden test with a temporary file
golden               :: Eq golden => String -> Golden actual golden -> actual -> Spec
golden name golden a = goldenTest name $ GoldenTest golden a

-- | Combinator to run a golden test with a temporary file
goldenTest         :: Eq golden => String -> GoldenTest actual golden -> Spec
goldenTest name gt = it name gt
