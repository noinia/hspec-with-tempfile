{-# LANGUAGE QuasiQuotes #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Test.Hspec.WithTempFile
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Support for easily running golden tests using temporary files
--
--------------------------------------------------------------------------------
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

-- | Run a golden test, where the golden files are all determined based on the name
-- of the test, and with respect to the given base dir.
goldenWith           :: Eq golden
                     => OsPath                -- ^ base dir containing golden files
                     -> Golden golden actual  -- ^ the test specification
                     -> actual                -- ^ the actual output
                     -> Spec
goldenWith baseDir t = goldenTest (t { goldenFile = baseDir </> fromTestName t.name })

-- | Runs a golden test exaclty as specified.
goldenTest     :: Eq golden => Golden golden actual -> actual -> Spec
goldenTest t x = rawGoldenTest $ GoldenTest t x

-- | Combinator to run a golden test as is.
rawGoldenTest    :: Eq golden => GoldenTest golden actual -> Spec
rawGoldenTest gt = do nameString <- runIO $ decodeFS gt.testSpec.name
                      it nameString gt
