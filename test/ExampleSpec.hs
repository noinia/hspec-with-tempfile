{-# LANGUAGE QuasiQuotes #-}
module ExampleSpec
  (spec
  ) where

import System.OsPath
import Test.Hspec
import Test.Hspec.WithTempFile

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "example test" $ do
         golden (byteStringGolden { name = [osp|fib_5.golden|] } )
                (fib 5)
         golden (byteStringGolden { name = [osp|fib_10.golden|] } )
                (fib 10)

-- | I would suggest to define some helper function liek this to incorporate the basedir.
golden  :: (Show actual, Eq golden)
        => Golden actual golden -> actual -> Spec
golden = goldenWith [osp|data/golden|]


fib   :: Int -> Int
fib n = fibs !! n
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
