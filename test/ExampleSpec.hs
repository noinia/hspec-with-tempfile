module ExampleSpec
  (spec
  ) where

import Test.Hspec
import qualified Test.Hspec.WithTempFile as WithTempFile

--------------------------------------------------------------------------------

spec :: Spec
spec = WithTempFile.spec
