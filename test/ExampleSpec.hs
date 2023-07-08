module ExampleSpec
  (spec
  ) where

import System.OsPath
import Test.Hspec
import Test.Hspec.WithTempFile
import Data.Default.Class

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "example test" $ do
         golden (byteStringGolden { name = [osp|fib 5.golden|] } )
                (fib 5)
         golden (def { name = [osp|fib 10.golden|] } )
                (fib 10)


-- |
golden                :: (Show actual, Eq golden)
                      => Golden actual golden -> actual -> Spec
golden = goldenWith [osp|data/golden|]


fib   :: Int -> Int
fib n = fibs !! n
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
