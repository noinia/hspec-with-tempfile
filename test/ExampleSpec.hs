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
         golden (byteStringGolden { name = [osp|fib 5.golden|] } )
                (fib 5)

-- |
golden  :: (Show actual, Eq golden)
        => Golden actual golden -> actual -> Spec
golden = goldenWith [osp|data/golden|]


fib   :: Int -> Int
fib n = fibs !! n
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
