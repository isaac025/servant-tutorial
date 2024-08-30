{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Prelude.Compat
import Prelude ()

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import qualified Data.Aeson.Parser
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Lucid
import Servant.Types.SourceT
import System.Directory
import Text.Blaze
import qualified Text.Blaze.Html
import Text.Blaze.Html.Renderer.Utf8

type UserAPI1 = "users" :> Get '[JSON] [User]

data User = User
    { name :: String
    , age :: Int
    , email :: String
    , registrationDate :: Day
    }
    deriving (Eq, Show, Generic)

instance ToJSON User

users1 :: [User]
users1 =
    [ User "Isaac Newton" 372 "isaac@newton.co.uk" (fromGregorian 1683 3 1)
    , User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)
    ]

{- Server type family computes the right type that a bunch of requests handlers
 - should have just from the corresponding API type.
 - 1. Server type family behind the scenes drives the routing, focus business logic
 - 2. Handlers will by default run in the Handler monad (overridable easily)
 - 3. Type value returned in that monad must be equal to the second arg of the
 - HTTP method combinator. In UserAPI1 our handler type must be Handler [User]
-}

server1 :: Server UserAPI1
server1 = pure users1

userAPI :: Proxy UserAPI1
userAPI = Proxy -- used to guide type inference

app1 :: Application
app1 = serve userAPI server1

type UserAPI2 =
    "users" :> Get '[JSON] [User]
        :<|> "albert" :> Get '[JSON] User
        :<|> "isaac" :> Get '[JSON] User

isaac :: User
isaac = User "Isaac Newton" 372 "isaac@newton.co.uk" (fromGregorian 1683 3 1)

albert :: User
albert = User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)

users2 :: [User]
users2 = [isaac, albert]

-- Just like we separate our endpoints we separate our handlers
-- with :<|> combinator!
-- Pitfall: Must be in the same order as the API
server2 :: Server UserAPI2
server2 =
    pure users2
        :<|> pure albert
        :<|> pure isaac

userAPI2 :: Proxy UserAPI2
userAPI2 = Proxy

app2 :: Application
app2 = serve userAPI2 server2

-- Lets use fancy combinators like QueryParam, Capture and ReqBody.
-- NOTE: You never have to worry for looking up a URL manually or query string
-- parameters or decoding/encoding data from/to JSON. NEVER!

type API =
    "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
        :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
        :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email

data Position = Position
    { xCoord :: Int
    , yCoord :: Int
    }
    deriving (Generic)

instance ToJSON Position

newtype HelloMessage = HelloMessage {msg :: String}
    deriving (Generic)

instance ToJSON HelloMessage

data ClientInfo = ClientInfo
    { clientName :: String
    , clientEmail :: String
    , clientAge :: Int
    , clientInterestedIn :: [String]
    }
    deriving (Generic)

instance FromJSON ClientInfo
instance ToJSON ClientInfo

data Email = Email
    { from :: String
    , to :: String
    , subject :: String
    , body :: String
    }
    deriving (Generic)

instance ToJSON Email

emailForClient :: ClientInfo -> Email
emailForClient c = Email from' to' subject' body'
  where
    from' = "great@company.com"
    to' = clientEmail c
    subject' = "Hey " ++ clientName c ++ ", we miss you!"
    body' =
        "Hi "
            ++ clientName c
            ++ ",\n\n"
            ++ "Since you've recently turned "
            ++ show (clientAge c)
            ++ ", have you checked out our latest "
            ++ intercalate ", " (clientInterestedIn c)
            ++ " products? Give us a visit!"

-- You can see how the types for the handler to what we need
server3 :: Server API
server3 = position :<|> hello :<|> marketing
  where
    position :: Int -> Int -> Handler Position
    position x y = pure (Position x y)

    hello :: Maybe String -> Handler HelloMessage
    hello Nothing = pure $ HelloMessage "Hello, anonymous coward"
    hello (Just n) = pure $ HelloMessage ("Hello, " ++ n)

    marketing :: ClientInfo -> Handler Email
    marketing clientinfo = pure (emailForClient clientinfo)

api :: Proxy API
api = Proxy

app3 :: Application
app3 = serve api server3

{- For reference:
 - HTTP Verbs do not become arguments, they provide the return type
 - Capture "something" a becimes an arg of type a
 - QueryParam "something" a become arg of type Maybe a
 - Header "something" a become arg of type Maybe a
 - QueryFlag "something" gets turned into an arg of type Bool
 - QueryParams "something" a gets turned into type [a]
 - ReqBody contentTypes a gets turned to arg of type a
-}

type PersonAPI = "persons" :> Get '[JSON, HTML] [Person]

data Person = Person
    { firstName :: String
    , lastName :: String
    }
    deriving (Generic)

instance ToJSON Person

instance ToHtml Person where
    toHtml person =
        tr_ $ do
            td_ (toHtml $ firstName person)
            td_ (toHtml $ lastName person)
    toHtmlRaw = toHtml

instance ToHtml [Person] where
    toHtml persons = table_ $ do
        tr_ $ do
            th_ "first name"
            th_ "last name"
        foldMap toHtml persons
    toHtmlRaw = toHtml

people :: [Person]
people = [Person "Isaac" "Newton", Person "Albert" "Einstein"]

personAPI :: Proxy PersonAPI
personAPI = Proxy

server4 :: Server PersonAPI
server4 = pure people

app4 :: Application
app4 = serve personAPI server4
