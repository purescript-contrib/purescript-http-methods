module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.HTTP.Method (Method(..), fromString, print, unCustomMethod)
import Effect (Effect)

foreign import fail :: String -> Effect Unit

assertEqual :: forall a. Eq a => Show a => String -> a -> a -> Effect Unit
assertEqual label expected actual =
  unless (actual == expected) do
    fail (label <> ": expected " <> show expected <> ", got " <> show actual)

main :: Effect Unit
main = do
  assertEqual "show QUERY" "QUERY" (show QUERY)
  assertEqual "print QUERY" "QUERY" (print (Left QUERY))
  assertEqual "parse uppercase QUERY" (Left QUERY) (fromString "QUERY")
  assertEqual "parse lowercase QUERY" (Left QUERY) (fromString "query")
  assertEqual "parse mixed-case QUERY" (Left QUERY) (fromString "QuErY")
  case fromString "FROB" of
    Left method -> fail ("unknown method parsed as " <> show method)
    Right custom -> assertEqual "unknown method" "FROB" (unCustomMethod custom)
