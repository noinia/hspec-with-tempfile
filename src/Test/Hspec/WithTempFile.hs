module Test.Hspec.WithTempFile
  ( golden



  , spec
  ) where

import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import           Test.Hspec


import           Test.Hspec.Core.Spec
--------------------------------------------------------------------------------

fib   :: Int -> Int
fib n = fibs !! n
  where
    fibs = 0 : zipWith (+) fibs (tail fibs)


spec :: Spec
spec = describe "example test" $ do
         golden "simple Text test" $ myTest

--          goldenWith "simple Text tests" defGolden (fib 5)

-- goldenWith          :: String -> Golden golden actual -> actual ->
-- goldenWith name g x = it name $ runGolden g x

golden         :: String -> GoldenTest golden actual -> Spec
golden name gt = it name gt

data GoldenTest golden actual =
  GoldenTest { goldenTest :: Golden golden actual
             , testOutput :: actual
             }

instance Example (GoldenTest golden actual) where
  evaluateExample gt param act prog = pure $ Result "skipped" Success



myTest = GoldenTest defGolden (fib 5)


-- instance Example (Golden golden actual) where
--   -- type Arg (actual -> Golden golden actual) = ()

--   evaluateExample g param act prog = runGolden g


-- data Result = SameOutput
--             | OutputMissmatch
--             deriving (Show,Eq)

runGolden :: Golden golden actual -> actual -> IO Result
runGolden = undefined






data Diff g = Diff { expected :: g
                   , actual   :: g
                   }
            deriving (Show,Eq,Functor,Foldable,Traversable)

data Golden golden actual =
  Golden { testWriter :: TestWriter golden actual
         -- ^ how to read the golden file
         , goldenFile :: FilePath
          -- ^ how to write the actual test output to the given file
         , readGoldenFile :: FilePath -> IO golden
         -- ^ the filePath that stores the golden output
         , actualFile :: ActualFile
         -- ^ file path where/how to store the test output
         , prettyActual     :: actual -> String
         -- ^ in case the test fails, how to show the actual input
         , prettyGoldenDiff :: Diff golden -> String
         -- ^ in case the test fails, how to show the difference between the exected and
         -- actual outputs.
         }

-- | The default way of showing the result of golden tests
defGolden :: Show a => Golden Text.Text a
defGolden = Golden { testWriter = WriteActual $ \fp -> TIO.writeFile fp . Text.pack . show
                   , goldenFile = "defGolden.golden"
                   , readGoldenFile = TIO.readFile
                   , actualFile = KeepOnFailure "defGolden.actual"
                   , prettyActual = show
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
data ActualFile = Discard
                -- ^ always discard the actual file, no matter what the test outcome
                | KeepOnFailure FilePath
                  -- ^ only keep the output file on failure
                | KeepAlways    FilePath
                  -- ^ always (i.e. independent of the test output) keep it
                deriving (Show,Read,Eq)

-- | Writeing a test to the
data TestWriter golden actual = WriteActual (FilePath -> actual -> IO ())
                              -- ^ procedure to write your actual test output to a file
                              | WriteGolden (FilePath -> golden -> IO ())
                              -- ^ procedure to write a specific golden output to a file.
                              -- this way you can manually convert the testouput into a
                              -- 'golden' before you write it to file as well.

-- todo; turn into an Either + pattern synonyms



-- goldenWith :: FilePath -- ^ base path
--            ->
