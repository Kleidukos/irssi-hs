module Main (main) where

-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
import           Data.Aeson
import qualified Data.Vector as V
import           Test.Hspec

import           Irssi.Types

main :: IO ()
main = hspec specs

specs :: Spec
specs = parallel $ do
    it "is trivially true" $ True `shouldBe` True
    it "JSON encoding for a Message is valid" $ do
      let command = Message "msg" "freenode" "#bottest" "Hey"
      encode command `shouldBe` "[\"command\",\"msg -freenode #bottest Hey\"]"
    it "JSON encoding for a Print command is valid" $ do
      let command = Print "Status: OK"
      encode command `shouldBe` "[\"Irssi::print\",\"Status: OK\"]"
    it "Decoding chatnets message" $ do
      let result = eitherDecodeStrict chatnetJSONPayload
      result `shouldBe` Right chatnetResponse

chatnetJSONPayload :: ByteString
chatnetJSONPayload = "[\"done\",[\"Irssi::Irc::Chatnet\",{\"_irssi\":94515019796528,\"chat_type\":\"IRC\",\"name\":\"DALnet\",\"type\":\"CHATNET\"}],[\"Irssi::Irc::Chatnet\",{\"_irssi\":94515019788944,\"chat_type\":\"IRC\",\"name\":\"EFNet\",\"type\":\"CHATNET\"}]]"

chatnetResponse :: ChatnetResponse
chatnetResponse = CR $ V.fromList [Chatnet {chatType = IRC, name = "DALnet", nick = Nothing, username = Nothing, realname = Nothing, ownHost = Nothing, autosendcmd = Nothing, servers = Nothing},Chatnet {chatType = IRC, name = "EFNet", nick = Nothing, username = Nothing, realname = Nothing, ownHost = Nothing, autosendcmd = Nothing, servers = Nothing}]
