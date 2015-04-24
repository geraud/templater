# Templater

Simple string templater

## Installation

```bash
cabal update
cabal install templater
```

## Example

```haskell
{-# LANGUAGE OverloadedStrings #-}
import           Data.Text
import qualified Data.Text.IO   as TIO
import           Text.Templater

main :: IO ()
main = do
  let textTemplate = "Hello, %{what is it ?}!"
      res = template textTemplate context
  case res of
    Left error -> do
        putStrLn $ "Got Error:" ++ error
    Right result -> TIO.putStrLn result

context :: Context -- Context is a type alias for Text -> Maybe Text
context "what is it ?" = Just "world"
context _ = Nothing
```
renders to `Hello, world!`

Well that's all there is to it!...
