# irssi-hs [![Simple Haskell][simple haskell]](https://www.simplehaskell.org) ![Haskell CI][ci]

This library allows you to speak to irssi through the `Irssi-Instance` irssi plugin.  

:warning: This is a work in progress

## Usage

Start the worker that will speak to the socket

```haskell
import Irssi

main :: IO ()
main = do
  let command = Msg Message{cmd="msg", network="freenode", channel="#bottest", message="I am alive!"}
  Right irsiState <- runExceptT (startWorker "/home/foo/.irssi.sock")
  sendMessage command (stargate irssiState)
```

[simple haskell]: https://www.simplehaskell.org/badges/badge.svg
[hackage]: https://img.shields.io/hackage/v/irssi-hs.svg
[ci]: https://github.com/kleidukos/irssi-hs/workflows/Haskell%20CI/badge.svg
