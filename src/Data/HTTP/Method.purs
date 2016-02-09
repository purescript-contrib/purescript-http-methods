module Data.HTTP.Method
  ( Method(..)
  , CustomMethod()
  , runCustomMethod
  , fromString
  , print
  ) where

import Prelude

import Data.Either (Either(..), either)
import Data.Generic (Generic, gCompare)
import Data.String as Str

data Method
  -- HTTP/1.1
  = OPTIONS
  | GET
  | HEAD
  | POST
  | PUT
  | DELETE
  | TRACE
  | CONNECT

  -- RFC 2518
  | PROPFIND
  | PROPPATCH
  | MKCOL
  | COPY
  | MOVE
  | LOCK
  | UNLOCK

  -- RFC5789
  | PATCH

derive instance genericMethod :: Generic Method

instance eqMethod :: Eq Method where
  eq OPTIONS OPTIONS = true
  eq GET GET = true
  eq HEAD HEAD = true
  eq POST POST = true
  eq PUT PUT = true
  eq DELETE DELETE = true
  eq TRACE TRACE = true
  eq CONNECT CONNECT = true
  eq PROPFIND PROPFIND = true
  eq PROPPATCH PROPPATCH = true
  eq MKCOL MKCOL = true
  eq COPY COPY = true
  eq MOVE MOVE = true
  eq LOCK LOCK = true
  eq UNLOCK UNLOCK = true
  eq PATCH PATCH = true
  eq _ _ = false

instance ordMethod :: Ord Method where
  compare = gCompare

instance showMethod :: Show Method where
  show OPTIONS = "OPTIONS"
  show GET = "GET"
  show HEAD = "HEAD"
  show POST = "POST"
  show PUT = "PUT"
  show DELETE = "DELETE"
  show TRACE = "TRACE"
  show CONNECT = "CONNECT"
  show PROPFIND = "PROPFIND"
  show PROPPATCH = "PROPPATCH"
  show MKCOL = "MKCOL"
  show COPY = "COPY"
  show MOVE = "MOVE"
  show LOCK = "LOCK"
  show UNLOCK = "UNLOCK"
  show PATCH = "PATCH"

newtype CustomMethod = CustomMethod String

runCustomMethod :: CustomMethod -> String
runCustomMethod (CustomMethod m) = m

derive instance genericCustomMethod :: Generic CustomMethod

instance eqCustomMethod :: Eq CustomMethod where
  eq (CustomMethod m1) (CustomMethod m2) = m1 == m2

instance ordCustomMethod :: Ord CustomMethod where
  compare = gCompare

instance showCustomMethod :: Show CustomMethod where
  show (CustomMethod m) = "(CustomMethod " <> show m <> ")"

fromString :: String -> Either Method CustomMethod
fromString s =
  case Str.toUpper s of
    "OPTIONS" -> Left OPTIONS
    "GET" -> Left GET
    "HEAD" -> Left HEAD
    "POST" -> Left POST
    "PUT" -> Left PUT
    "DELETE" -> Left DELETE
    "TRACE" -> Left TRACE
    "CONNECT" -> Left CONNECT
    "PROPFIND" -> Left PROPFIND
    "PROPPATCH" -> Left PROPPATCH
    "MKCOL" -> Left MKCOL
    "COPY" -> Left COPY
    "MOVE" -> Left MOVE
    "LOCK" -> Left LOCK
    "UNLOCK" -> Left UNLOCK
    m -> Right (CustomMethod m)

print :: Either Method CustomMethod -> String
print = either show runCustomMethod
