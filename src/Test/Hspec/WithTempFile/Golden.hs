{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.Hspec.WithTempFile.Golden
  ( GoldenTest(..)
  , Golden(..)
  , defaultGolden

  , Diff(..)
  ) where

import           Control.Monad (when)
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as E
import           System.Directory.OsPath as Directory
import qualified System.File.OsPath as File
import qualified System.IO
import           System.OsPath
import           Test.Hspec.Core.Spec

--------------------------------------------------------------------------------

data GoldenTest actual golden =
  GoldenTest { theTest    :: Golden actual golden
             , testOutput :: actual
             }

instance Eq golden => Example (GoldenTest actual golden) where
  evaluateExample gt param act prog = runGoldenTest gt
  -- todo, do something with these other args

-- | Run the actual golden test
runGoldenTest                       :: Eq golden => GoldenTest actual golden -> IO Result
runGoldenTest (GoldenTest golden a) =
  do actualFileFP <- actualFilePath golden
     writeActual golden actualFileFP a
     expectedOut <- golden.readGoldenFile golden.goldenFile
     actualOut   <- golden.readGoldenFile actualFileFP
     if expectedOut == actualOut
       then do when (golden.actualFilePolicy /= KeepAlways) $ cleanup actualFileFP
               pure $ Result "golden test succeeded"  Success
       else do when (golden.actualFilePolicy /= Discard) $ cleanup actualFileFP
               let mLoc   = Nothing -- location?
                   reason = mkReason golden a $ Diff expectedOut actualOut
               pure $ Result "golden test failed" $ Failure mLoc reason

-- | clean up the golden file
cleanup :: OsPath -> IO ()
cleanup = Directory.removeFile

mkReason               :: Golden actual golden
                       -> actual -> Diff golden -> FailureReason
mkReason golden a diff = Reason . mconcat $
    [ "golden test for " <> golden.prettyActual a <> " failed since "
    , golden.prettyGoldenDiff diff
    ]

data Diff g = Diff { expected :: g
                   , actual   :: g
                   }
            deriving (Show,Eq,Functor,Foldable,Traversable)

-- | Function to write the test output to a file.
writeActual        :: Golden actual golden -> OsPath -> actual -> IO ()
writeActual golden = case golden.actualWriter of
                       Convert f     -> \fp -> golden.writeGolden fp . f
                       WriteActual g -> g


-- | How to write a test to the golden file.
data ActualWriter actual golden = Convert     (actual -> golden)
                                -- ^ convert the actual into a golden and then write it
                                -- using writeGolden
                                | WriteActual (OsPath -> actual -> IO ())
                                  -- ^ directly write the actual output to a file.




data ActualFile = TempFile      { tempDir :: Maybe OsPath
                                  -- ^ use the system default if Nothing
                                , nameTemplate :: OsString
                                  -- ^ file name to use in the tempfile. As with tempfile
                                  -- if your pattern is "foo.ext" the final filename will
                                  -- become "fooXYZ.ext", where XYZ is some random string.
                                }
                | PermanentFile OsPath -- ^ absolute path to a normal file
                deriving (Show,Eq)

defaultActualFile      :: OsString -> ActualFile
defaultActualFile name = TempFile Nothing name


-- TODO: a proper implementation of this should go in some module somewhere.
openTempFile           :: OsPath -> OsString -> IO (OsPath, System.IO.Handle)
openTempFile dir name = do dir'    <- decodeFS dir
                           name'   <- decodeFS name
                           (fp',h) <- System.IO.openTempFile dir' name'
                           fp      <- encodeFS fp'
                           pure (fp,h)

-- | Creates a temporary file (and closes it).
createTempFile          :: OsPath -> OsString -> IO OsPath
createTempFile dir name = do (fp,h) <- openTempFile dir name
                             System.IO.hClose h
                             pure fp

-- | Get the actual file path, making sure to create the directlry if it does not exist.
actualFilePath        :: Golden actual golden -> IO OsPath
actualFilePath golden = case golden.actualFile of
    TempFile mDir name -> do dir <- maybe Directory.getTemporaryDirectory pure mDir
                             createTempFile dir name
    PermanentFile fp   -> do Directory.createDirectoryIfMissing True fp
                             pure fp


-- flip argument order;
data Golden actual golden =
  Golden { actualWriter :: ActualWriter actual golden
         -- ^ how to write the actual file output onto disk
         , writeGolden :: OsPath -> golden -> IO ()
         -- ^ how to read the golden file
         , goldenFile :: OsPath
          -- ^ how to write the actual test output to the given file
         , readGoldenFile :: OsPath -> IO golden
         -- ^ the filePath that stores the golden output
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


-- readTextFileUtf8 :: OsPath -> IO Text.Text
-- readTextFileUtf8 =

-- | The default way of showing the result of golden tests
defaultGolden :: Show a => Golden a ByteString.ByteString
defaultGolden = Golden { actualWriter     = Convert $ Char8.pack . show
                       , writeGolden      = File.writeFile
                       , goldenFile       = [osp|defaultGolden.golden|]
                       , readGoldenFile   = File.readFile
                       , actualFile       = defaultActualFile [osp|defaultGolden.actual|]
                       , actualFilePolicy = KeepOnFailure
                       , prettyActual     = show
                       , prettyGoldenDiff = show
                       }


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



-- | What to do with the actual test output file
data ActualFilePolicy = Discard
                      -- ^ always discard the actual file, no matter what the test outcome
                      | KeepOnFailure
                      -- ^ only keep the output file on failure
                      | KeepAlways
                      -- ^ always (i.e. independent of the test output) keep it
                      deriving (Show,Read,Eq)




-- goldenWith :: OsPath -- ^ base path
--            ->
