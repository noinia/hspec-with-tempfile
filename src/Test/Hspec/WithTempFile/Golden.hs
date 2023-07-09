{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Test.Hspec.WithTempFile.Golden
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module Test.Hspec.WithTempFile.Golden
  ( Golden(..)
  , byteStringGolden
  , textGolden
  , dimapWith

  , GoldenFilePolicy(..)
  , ActualWriter(..)
  , ActualFile(..)
  , fromTestName
  , ActualFilePolicy(..)
  , Diff(..)

  , writeActual

  , GoldenTest(..)
  ) where

import           Control.Monad (when)
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Lazy.Char8 as Char8
import           Data.Functor.Contravariant
import           Data.Profunctor
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as E
import           System.Directory.OsPath as Directory
import qualified System.File.OsPath as File
import qualified System.IO
import           System.OsPath
import           Test.Hspec.Core.Spec

--------------------------------------------------------------------------------

-- | A specification of a golden test. In particular, a test where values of type 'actual'
-- are seriealized into something of type 'golden', and then written to a file. The
-- content of this file is then compared with the correct answer that is deserialized from
-- a 'goldenFile'.
data Golden golden actual =
  Golden { name :: OsString
         -- ^ description of the test name
         , actualWriter :: ActualWriter actual golden
         -- ^ how to write the actual file output onto disk
         , writeGolden :: OsPath -> golden -> IO ()
         -- ^ how to read the golden file
         , goldenFile :: OsPath
          -- ^ how to write the actual test output to the given file
         , readGoldenFile :: OsPath -> IO golden
         -- ^ the filePath that stores the golden output
         , goldenFilePolicy :: GoldenFilePolicy
         -- ^ what to do with the golden file, in particular if it is missing.
         , actualFile :: ActualFile
         -- ^ file where/how to store the test output
         , actualFilePolicy :: ActualFilePolicy
         -- ^ What to do with the actual file
         , prettyActual     :: actual -> String
         -- ^ in case the test fails, how to show the actual input
         , prettyGoldenDiff :: Diff golden -> String
         -- ^ in case the test fails, how to show the difference between the exected and
         -- actual outputs.
         }

instance Contravariant (Golden golden) where
  contramap f t = t { actualWriter = lmap f t.actualWriter
                    , prettyActual = t.prettyActual . f
                    }

-- instance Show actual => Default (Golden actual ByteString.ByteString) where
--   def = byteStringGolden


----------------------------------------
-- * Constructing golden tests

-- | The default way of showing the result of golden tests.
--
-- The main scheme is to convert your testOutput into a ByteString, and write this
-- bytestring to a temporary file. The expected test output (stored at goldenFile) is then
-- read, and the output that is written to the temporary file is compared with the golden
-- file.  If the output matches the temporary file is deleted. If the test fails, the
-- output is kept.
byteStringGolden :: Show testOutput
                 => Golden ByteString.ByteString testOutput
byteStringGolden = Golden { name             = [osp|"defaultGolden"|]
                          , actualWriter     = Convert $ Char8.pack . show
                          , writeGolden      = File.writeFile
                          , goldenFile       = [osp|defaultGolden.golden|]
                          , readGoldenFile   = File.readFile
                          , goldenFilePolicy = CopyWithSuffix -- FailOnMissing
                          , actualFile       = tempFile
                          , actualFilePolicy = KeepOnFailure
                          , prettyActual     = show
                          , prettyGoldenDiff = show
                          }

-- | Same as byteStringGolden, except that to compare the file contents it reads and writes
-- the data as UTF-8 encoded text.
textGolden :: Show testOutput => Golden  Text.Text testOutput
textGolden = dimapWith (\fp -> File.writeFile fp . E.encodeUtf8)
                       show
                       id
                       E.decodeUtf8
                       byteStringGolden

--------------------------------------------------------------------------------

-- | Convenience method to create test specifications from other ones.
dimapWith                   :: (OsPath -> golden' -> IO ())
                            -- ^ the new writeGolden implementation
                            -> (Diff golden' -> String)
                            -- ^ the new prettyDiff function
                            -> (actual' -> actual)
                            -> (golden -> golden')
                            -> Golden golden actual -> Golden golden' actual'
dimapWith writeGolden'
          prettyGoldenDiff'
          f g t             = Golden { name             = t.name
                                     , actualWriter     = dimap f g t.actualWriter
                                     , writeGolden      = writeGolden'
                                     , goldenFile       = t.goldenFile
                                     , readGoldenFile   = fmap g . t.readGoldenFile
                                     , goldenFilePolicy = t.goldenFilePolicy
                                     , actualFile       = t.actualFile
                                     , actualFilePolicy = t.actualFilePolicy
                                     , prettyActual     = t.prettyActual . f
                                     , prettyGoldenDiff = prettyGoldenDiff'
                                     }

--------------------------------------------------------------------------------
-- * Writing your actual test output to a file.

-- | How to write a test to the golden file.
data ActualWriter actual golden = Convert     (actual -> golden)
                                -- ^ convert the actual into a value of type golden, so
                                -- that we can write it to a file using writeGolden
                                | WriteActual (OsPath -> actual -> IO ())
                                  -- ^ directly write the actual output to a file.
                                deriving (Functor)

instance Profunctor ActualWriter where
  dimap f g = \case
    Convert h     -> Convert $ g . h . f
    WriteActual h -> WriteActual $ \fp -> h fp . f


--------------------------------------------------------------------------------

-- | Data type describing where to write the file to. Either to a temporary file or a
-- reglar one with a specific file path.
data ActualFile = TempFile      { tempDir :: Maybe OsPath
                                  -- ^ use the system default if Nothing
                                }
                | PermanentFile OsPath -- ^ absolute path to a normal file
                deriving (Show,Eq)

-- | convenience constructor for constructing a temp file in the OS tempdir.
tempFile :: ActualFile
tempFile = TempFile Nothing


-- | turn the test name into a somewhat reasonable fileName
fromTestName :: OsString -> OsPath
fromTestName = makeValid

--------------------------------------------------------------------------------

-- | What to do when the golden file is missing
data GoldenFilePolicy = FailOnMissing
                      | CopyWithSuffix -- ^ copies the actual file to 'nameOfTheGoldenFile.actual'
                      -- | AskInteractively
                      deriving (Show,Read,Eq)

-- | What to do with the actual test output file.
data ActualFilePolicy =
        Discard       -- ^ always discard the actual file, no matter what the test outcome
      | KeepOnFailure -- ^ only keep the output file on failure
      | KeepAlways    -- ^ always (i.e. independent of the test output) keep it
      deriving (Show,Read,Eq)

--------------------------------------------------------------------------------

-- | Specification of the expected output together with the received output.
data Diff a = Diff { expected :: a, actual :: a }
            deriving (Show,Eq,Functor,Foldable,Traversable)

--------------------------------------------------------------------------------
-- * Helper functions for the 'Golden' type, typically Extracting information from a
-- * 'Golden'

-- | Function to write the test output to a file.
writeActual        :: Golden golden actual -> OsPath -> actual -> IO ()
writeActual golden = case golden.actualWriter of
                       Convert f     -> \fp -> golden.writeGolden fp . f
                       WriteActual g -> g

-- | Get the actual file path, making sure to create the directory if it does not exist.
actualFilePath        :: Golden golden actual -> IO OsPath
actualFilePath golden = case golden.actualFile of
    TempFile mDir      -> do dir   <- maybe Directory.getTemporaryDirectory pure mDir
                             createTempFile dir $ fromTestName golden.name
    PermanentFile fp   -> do Directory.createDirectoryIfMissing True fp
                             pure fp

--------------------------------------------------------------------------------

-- | A Golden test specification together with a particular output
data GoldenTest golden actual =
  GoldenTest { testSpec    :: Golden golden actual
             -- ^ the specification of te test
             , testOutput :: actual
             -- ^ the output of the test
             }

instance Eq golden => Example (GoldenTest golden actual) where
  evaluateExample t _param _act _prog = runGoldenTest t
  -- todo, do something with these other args


-- | Run the actual golden test
runGoldenTest                       :: Eq golden => GoldenTest golden actual -> IO Result
runGoldenTest (GoldenTest golden a) =
  do actualFileFP <- actualFilePath golden
     writeActual golden actualFileFP a

     goldenExists <- Directory.doesFileExist golden.goldenFile
     res <- if goldenExists then pure Success
                            else handleGoldenMissing actualFileFP golden
     case res of
       Success -> testGoldenEquality actualFileFP -- we've fixed things, so test for equaltiy
       _       -> pure $ Result "golden file missing" res -- the file remains missing, so presumably fail


  where
    testGoldenEquality actualFileFP =
      do
        expectedOut <- golden.readGoldenFile golden.goldenFile
        actualOut   <- golden.readGoldenFile actualFileFP
        if expectedOut == actualOut
          then do when (golden.actualFilePolicy /= KeepAlways) $ cleanup actualFileFP
                  pure $ Result "golden test succeeded"  Success
          else do when (golden.actualFilePolicy /= Discard) $ cleanup actualFileFP
                  loc <- actualFileLoc actualFileFP
                  let mLoc   = Just loc
                      reason = mkReason golden a $ Diff expectedOut actualOut
                  pure $ Result "golden test failed" $ Failure mLoc reason

-- | Turn the path to the actual file into a 'Location'.
actualFileLoc              :: OsPath -> IO Location
actualFileLoc actualFileFP = (\actualFileFP' -> Location actualFileFP' 0 0)
                            <$> decodeFS actualFileFP

handleGoldenMissing                 :: OsPath
                                    -- ^ file path to the actual file
                                    -> Golden golden actual
                                    -- ^ file path to the golden file
                                    -> IO ResultStatus
                                    -- ^ return whether we should contine or that things
                                    -- are already broken.
handleGoldenMissing actualFP golden = case golden.goldenFilePolicy of
  FailOnMissing  -> failWithLocation actualFP golden.goldenFile
  CopyWithSuffix -> copyWithSuffix actualFP golden.goldenFile
  -- AskInteractively -> interactivelyCopy actualFP golden.goldenFile


copyWithSuffix                   :: OsPath -> OsPath -> IO ResultStatus
copyWithSuffix actualFP goldenFP = do Directory.copyFile actualFP (goldenFP <.> [osp|actual|])
                                      failWithLocation actualFP goldenFP

failWithLocation                   :: OsPath -> OsPath -> IO ResultStatus
failWithLocation actualFP goldenFP =
  (flip failMissingGoldenFile goldenFP) <$> actualFileLoc actualFP

-- | Failure message for when the golden file is missing
failMissingGoldenFile        :: Location -> OsPath -> ResultStatus
failMissingGoldenFile loc fp = Failure (Just loc) $ Reason ("missing golden file " <> show fp)


-- | keep asking whether to accept the current result until we anser y or n
_interactivelyCopy                   :: OsPath -> OsPath -> IO ResultStatus
_interactivelyCopy actualFP goldenFP = ask
  where
    ask = do
            putStrLn message
            getLine >>= \case
              "y" -> installGoldenFile
              "n" -> failWithLocation actualFP goldenFP
              _   -> ask

    message = unlines [ "Golden file " <> show goldenFP <> " missing!"
                      , "Install the actual output file " <> show actualFP <> " as golden file?"
                      , "y/n"
                      ]

    installGoldenFile = do Directory.copyFile actualFP goldenFP
                           pure Success


-- | clean up the golden file
cleanup :: OsPath -> IO ()
cleanup = Directory.removeFile

mkReason               :: Golden golden actual
                       -> actual -> Diff golden -> FailureReason
mkReason golden a diff = Reason . mconcat $
    [ "golden test with output " <> golden.prettyActual a <> " failed since "
    , golden.prettyGoldenDiff diff
    ]

--------------------------------------------------------------------------------
-- * Generic Helper implementations

-- | Creates a temporary file (and closes it).
createTempFile          :: OsPath -> OsString -> IO OsPath
createTempFile dir name = do (fp,h) <- openTempFile dir name
                             System.IO.hClose h
                             pure fp

-- | Todo, this should go into some library somewhere
-- moreover, we should implement this properly rather than just decoding the OsPath
--
openTempFile           :: OsPath -> OsString -> IO (OsPath, System.IO.Handle)
openTempFile dir name = do dir'    <- decodeFS dir
                           name'   <- decodeFS name
                           (fp',h) <- System.IO.openTempFile dir' name'
                           fp      <- encodeFS fp'
                           pure (fp,h)




-- readTextFileUtf8 :: OsPath -> IO Text.Text
-- readTextFileUtf8 =

-- ipeGoldenWith           :: OsPath -- ^ base file path
--                      -> String  -- ^ name of the specific test; we append this to the base filepath
--                      -> a
--                      -> Golden a
-- ipeGoldenWith baseFp name actual =
--   Golden { output = actual
--          , encodePretty = show
--          , writeToFile :: OsPath -> str -> IO ()
--          , readFromFile :: OsPath -> IO str
--          , goldenFile = baseFP <.> name <.> "out.ipe"
--          , actualFile = Just $ baseFP <.> name <.> "out.actual.ipe"
--          , failFirstTime = True
--          }
