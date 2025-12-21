{-# LANGUAGE QuasiQuotes #-}
module ExampleSpec
  (spec
  ) where

import           System.OsPath
import           Test.Hspec
import           Test.Hspec.WithTempFile
import qualified System.File.OsPath as File
-- import qualified Data.ByteString.Lazy.Char8 as Char8

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "example test" $ do
         golden (byteStringGolden { name = [osp|fib_5|] } )
                (fib 5)
         golden (byteStringGolden { name = [osp|fib_10|] } )
                (fib 10)
         golden (byteStringGolden { name = [osp|fib_11|] } )
                (fib 11)

         str <- runIO $ File.readFile [osp|data/golden/same.ipe|]
         golden (byteStringGolden { name         = [osp|same.ipe|]
                                  , actualWriter = WriteActual $ File.writeFile
                                  } )
                str

         {-
         str' <- runIO $ File.readFile [osp|data/golden/manual.ipe|]
         golden (byteStringGolden { name = [osp|manual.ipe|]
                                  , actualWriter = WriteActual $ File.writeFile
                                  } )
                str'
          -}

-- | I would suggest to define some helper function like this to incorporate the basedir.
golden  :: (Show actual, Eq golden)
        => Golden golden actual -> actual -> Spec
golden = goldenWith [osp|data/golden|]


fib   :: Int -> Int
fib n = fibs !! n
  where
    fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)
