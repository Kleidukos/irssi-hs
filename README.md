# irssi-hs

This library allows you to speak to irssi through the `Irssi-Instance` irssi plugin.  

## Usage

First, initialie the state of your irssi instance with `initState :: Either Text IrssiState`

```haskell
import Irssi

Right irssiState <- initState
```
The library will then query your irssi for its capabilities (methods, chatnets, etc)
