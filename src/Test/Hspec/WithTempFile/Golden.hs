module Test.Hspec.WithTempFile.Golden
  ( GoldenTest(..)
  , Golden(..)
  , defGolden
  , Diff(..)
  ) where

import           Control.Monad (when)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import           Test.Hspec.Core.Spec

--------------------------------------------------------------------------------

data GoldenTest actual golden =
  GoldenTest { goldenTest :: Golden actual golden
             , testOutput :: actual
             }

instance Eq golden => Example (GoldenTest actual golden) where
  evaluateExample gt param act prog = runGoldenTest gt

-- | Run the actual golden test
runGoldenTest                            :: Eq golden => GoldenTest actual golden -> IO Result
runGoldenTest (GoldenTest golden a) =
  do writeActual golden golden.actualFile a
     expectedOut <- golden.readGoldenFile golden.goldenFile
     actualOut   <- golden.readGoldenFile golden.actualFile
     if expectedOut == actualOut
       then do cleanup golden.actualFile
               pure $ Result "golden test succeeded"  Success
       else do when (golden.actualFilePolicy /= Discard) $ cleanup golden.actualFile
               let mLoc   = Nothing -- location?
                   reason = mkReason golden a undefined -- $ Diff expectedOut actualOut
               pure $ Result "golden test failed" $ Failure mLoc reason

cleanup    :: FilePath
           -> IO ()
cleanup fp = pure ()

mkReason               :: Golden actual golden
                       -> actual
                       -> Diff golden -> FailureReason
mkReason golden a diff = Reason . mconcat $
    [ "golden test for " <> golden.prettyActual a <> " failed since "
    , golden.prettyGoldenDiff diff
    ]

data Diff g = Diff { expected :: g
                   , actual   :: g
                   }
            deriving (Show,Eq,Functor,Foldable,Traversable)

writeActual        :: Golden actual golden -> FilePath -> actual -> IO ()
writeActual golden = case golden.actualWriter of
                       Convert f     -> \fp -> golden.writeGolden fp . f
                       WriteActual g -> g

-- | Writeing a test to the
data ActualWriter actual golden = Convert     (actual -> golden)
                                | WriteActual (FilePath -> actual -> IO ())

-- flip argument order;
data Golden actual golden =
  Golden { actualWriter :: ActualWriter actual golden
         -- ^ how to write the actual file output onto disk
         , writeGolden :: FilePath -> golden -> IO ()
         -- ^ how to read the golden file
         , goldenFile :: FilePath
          -- ^ how to write the actual test output to the given file
         , readGoldenFile :: FilePath -> IO golden
         -- ^ the filePath that stores the golden output
         , actualFile :: FilePath
         -- ^ file path where/how to store the test output
         , actualFilePolicy :: ActualFilePolicy
         -- ^ What to do with the actual file
         , prettyActual     :: actual -> String
         -- ^ in case the test fails, how to show the actual input
         , prettyGoldenDiff :: Diff golden -> String
         -- ^ in case the test fails, how to show the difference between the exected and
         -- actual outputs.
         }

-- | The default way of showing the result of golden tests
defGolden :: Show a => Golden a Text.Text
defGolden = Golden { actualWriter     = Convert $ Text.pack . show
                   , writeGolden      = TIO.writeFile
                   , goldenFile       = "defGolden.golden"
                   , readGoldenFile   = TIO.readFile
                   , actualFile       = "defGolden.actual"
                   , actualFilePolicy = KeepOnFailure
                   , prettyActual     = show
                   , prettyGoldenDiff = show
                   }


-- ipeGoldenWith           :: FilePath -- ^ base file path
--                      -> String  -- ^ name of the specific test; we append this to the base filepath
--                      -> a
--                      -> Golden a
-- ipeGoldenWith baseFp name actual =
--   Golden { output = actual
--          , encodePretty = show
--          , writeToFile :: FilePath -> str -> IO ()
--          , readFromFile :: FilePath -> IO str
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




-- goldenWith :: FilePath -- ^ base path
--            ->
