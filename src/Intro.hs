{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Intro where

import Data.Text
import Data.Time (UTCTime)
import Servant.API

type UserAPI = "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]

data SortBy = Age | Name

data User = User
    { name :: String
    , age :: Int
    , email :: String
    , registrationDate :: UTCTime
    }

-- If / is an endpoint do not use :> combinator, example:
type RootEndpoint = Get '[JSON] User

-- Describe multiple endpoints with :<|>
type UserAPI2 =
    "users" :> "list-all" :> Get '[JSON] [User]
        :<|> "list-all" :> "users" :> Get '[JSON] [User]

{- COMBINATORS -}

-- static strings
type UserAPI3 = "users" :> "list-all" :> "now" :> Get '[JSON] [User]

-- Delete, Get, Patch, Post, Put Verbs of 200,
-- PostCreated and PostAccepted Verb of 201 and 202

-- Endpoint always ends with verb
type UserAPI4 =
    "users" :> Get '[JSON] [User]
        :<|> "admins" :> Get '[JSON] [User]

-- Capture combinator used to capture segments of url example
type UserAPI5 =
    "user" :> Capture "userid" Integer :> Get '[JSON] User
        :<|> "user" :> Capture "userid" Integer :> DeleteNoContent

-- QueryParam, QueryParams, QueryFlag parameters in url after ?, example:
-- /users?sortby=age
-- QueryParams queries a list of values e.g. ?param=val1&param=val2
-- QueryFlag is a boolean-like query e.g. /user?active
type UserAPI6 = "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]

-- ReqBody combinator that takes a list of content encoded in the request body
-- No need for header checking servant does it and return BadRequest or UnsupportedContentType
type UserAPI7 =
    "users" :> ReqBody '[JSON] User :> Post '[JSON] User
        :<|> "users"
            :> Capture "userid" Integer
            :> ReqBody '[JSON] User
            :> Put '[JSON] User

-- (Request) Header combinator takes a string for the header name and type to which decode
type UserAPI8 = "users" :> Header "User-Agent" Text :> Get '[JSON] [User]

-- Four content types out of the box:
-- JSON, PlainText, FormUrlEncoded and OctetStream
-- and you can use them all!
type UserAPI9 = "users" :> Get '[JSON, PlainText, FormUrlEncoded, OctetStream] [User]

-- (Response) Headers combinator carries a list of header types for a response
type UserAPI10 = "users" :> Get '[JSON] (Headers '[Header "User-Count" Integer] [User])

-- BasicAuth combinator specifies the realm and the datatype returned
type ProtectedAPI11 =
    UserAPI -- public
        :<|> BasicAuth "my-realm" User :> UserAPI2 -- protected

-- Empty APIs useful for generalization
type UserAPI12 innerAPI = UserAPI :<|> "inner" :> innerAPI

-- EmptyAPI combinator when nothing extra to serve, equal to ()
type UserAPI12Alone = UserAPI12 EmptyAPI

-- Raw combinator hatch for low-level web library wai
type UserAPI13 =
    "users" :> Get '[JSON] [User]
        :<|> Raw

-- Pitfall: Raw will match anything that hasn't been composed
type UserAPI14 =
    Raw
        :<|> "users" :> Get '[JSON] [User] -- never reached because Raw is first

-- Use Raw as last definition or static path
type UserAPI15 =
    "files" :> Raw -- raw matches after /files
        :<|> "users" :> Get '[JSON] [User]

type UserAPI16 = "users" :> Get '[JSON] [User] :<|> Raw -- raw is matched last
