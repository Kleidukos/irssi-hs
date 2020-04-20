module Main where
-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import qualified Test.Tasty
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
import           Data.Aeson
import           Test.Tasty.Hspec

import           Irssi.Types

main :: IO ()
main = do
    test <- testSpec "irssi-hs" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do
    it "is trivially true" $ do
        True `shouldBe` True
    it "JSON encoding is valid" $ do
      let command = Command "msg" "freenode" "#bottest" "Hey"
      encode command `shouldBe` "[\"command\",\"msg -freenode #bottest Hey\"]"
