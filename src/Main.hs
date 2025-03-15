module Main where

data Example = Example
  { name :: Text
  , age :: Int
  }
  deriving stock (Show, Eq)

{- |
 Main entry point.

 `just run` will invoke this function.
-}
main :: IO ()
main = do
  putTextLn "Hello 🌎 (from devden-stat-bot)"
