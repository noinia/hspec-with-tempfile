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

myTest = GoldenTest defGolden (fib 5)

spec :: Spec
spec = describe "example test" $ do
         golden "simple Text test" $ myTest

-- | Combinator to run a golden test with a temporary file
golden         :: Eq golden => String -> GoldenTest actual golden -> Spec
golden name gt = it name gt
