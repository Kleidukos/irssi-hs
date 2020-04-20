module Main (main) where

-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import qualified Test.Tasty
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector         as V
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
    it "JSON encoding for a Message is valid" $ do
      let command = Message "msg" "freenode" "#bottest" "Hey"
      encode command `shouldBe` "[\"command\",\"msg -freenode #bottest Hey\"]"
    it "Proplerly extract objects from nested array" $ do
      let obj1 = Object (HM.fromList [("_irssi",Number 9.4515019796528e13),("name",String "DALnet"),("type",String "CHATNET"),("chat_type",String "IRC")])
      extractObject (Array $ V.fromList [String "Irssi::Irc::Chatnet", obj1])
        `shouldBe` obj1
    it "Decoding chatnets message" $ do
      let result = eitherDecodeStrict chatnetJSONPayload
      result `shouldBe` Right chatnetResponse

chatnetJSONPayload :: ByteString
chatnetJSONPayload = "[\"done\",[\"Irssi::Irc::Chatnet\",{\"_irssi\":94515019796528,\"chat_type\":\"IRC\",\"name\":\"DALnet\",\"type\":\"CHATNET\"}],[\"Irssi::Irc::Chatnet\",{\"_irssi\":94515019788944,\"chat_type\":\"IRC\",\"name\":\"EFNet\",\"type\":\"CHATNET\"}]]"

chatnetResponse :: ChatnetResponse
chatnetResponse = CR $ V.fromList [Chatnet {chatType = IRC, name = "DALnet", nick = Nothing, username = Nothing, realname = Nothing, own_host = Nothing, autosendcmd = Nothing, servers = Nothing},Chatnet {chatType = IRC, name = "EFNet", nick = Nothing, username = Nothing, realname = Nothing, own_host = Nothing, autosendcmd = Nothing, servers = Nothing}]
