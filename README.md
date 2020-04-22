# irssi-hs [![Simple Haskell][simple haskell]](https://www.simplehaskell.org) ![Haskell CI][ci]

This library allows you to speak to irssi through the `Irssi-Instance` irssi plugin.  

:warning: This is a work in progress

## Usage

Initialise the state of your irssi instance with `initState :: Either Text IrssiState`

```haskell
import Irssi

main :: IO ()
main = do
  Right irssiState <- initState
```

[simple haskell]: https://www.simplehaskell.org/badges/badge.svg
[hackage]: https://img.shields.io/hackage/v/irssi-hs.svg
[ci]: https://github.com/kleidukos/irssi-hs/workflows/Haskell%20CI/badge.svg
