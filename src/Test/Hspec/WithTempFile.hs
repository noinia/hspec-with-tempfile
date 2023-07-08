{-# LANGUAGE QuasiQuotes #-}
module Test.Hspec.WithTempFile
  ( goldenWith
  , goldenTest

  , Golden(..)
  , byteStringGolden
  , textGolden
  , dimapWith

  , ActualWriter(..)
  , ActualFile(..)
  , fromTestName
  , ActualFilePolicy(..)

  , Diff(..)

  , rawGoldenTest
  , GoldenTest(..)
  ) where

import           System.OsPath
import           Test.Hspec
import           Test.Hspec.WithTempFile.Golden

--------------------------------------------------------------------------------

-- | Run a golden golden test, where the golden files are all determined based on the name
-- of the test, and with respect to the given base dir.
goldenWith           :: Eq golden
                     => OsPath                -- ^ base dir containing golden files
                     -> Golden actual golden  -- ^ the test specification
                     -> actual                -- ^ the actual output
                     -> Spec
goldenWith baseDir t = goldenTest (t { goldenFile = baseDir </> fromTestName t.name })

-- | Runs a golden test exaclty as specified.
goldenTest     :: Eq golden => Golden actual golden -> actual -> Spec
goldenTest t x = rawGoldenTest $ GoldenTest t x

-- | Combinator to run a golden test as is.
rawGoldenTest    :: Eq golden => GoldenTest actual golden -> Spec
rawGoldenTest gt = do nameString <- runIO $ decodeFS gt.testSpec.name
                      it nameString gt
