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
import Servant.Types.SourceT (source)
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
instance FromJSON User

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

-- Handler is newtype a wrapper around ExceptT ServerError IO
-- we can return success or fail with throwError
-- Handler == IO (Either ServerError a)
-- to run any IO computation just use liftIO
type IOAPI1 = "myfile.txt" :> Get '[JSON] FileContent

newtype FileContent = FileContent {content :: String}
    deriving (Generic)

instance ToJSON FileContent

server5 :: Server IOAPI1
server5 = do
    fileContent <- liftIO (readFile "myfile.txt")
    pure (FileContent fileContent)

ioAPI1 :: Proxy IOAPI1
ioAPI1 = Proxy

app5 :: Application
app5 = serve ioAPI1 server5

-- ServerError lets you explicitly fail by using throwError
-- ServerError lets you provide:
-- HTTP Status code
-- error message
-- error body
-- error headers
failingHandler :: Handler ()
failingHandler = throwError myerr
  where
    myerr :: ServerError
    myerr = err503{errBody = "Sorry dear user."}

server6 :: Server IOAPI1
server6 = do
    exists <- liftIO (doesFileExist "myfile.txt")
    if exists
        then liftIO (readFile "myfile.txt") >>= pure . FileContent
        else throwError custom404err
  where
    custom404err = err404{errBody = "myfile.txt just isn't there, please leave this server alone."}

app6 :: Application
app6 = serve ioAPI1 server6

-- Adding headers to a response changes the type of your API also
type MyHandler = Get '[JSON] (Headers '[Header "X-An-Int" Int] User)

myHandler :: Server MyHandler
myHandler = pure $ addHeader 1797 albert

myHandlerAPI :: Proxy MyHandler
myHandlerAPI = Proxy

app7 :: Application
app7 = serve myHandlerAPI myHandler

type MyHeadfulHandler = Get '[JSON] (Headers '[Header "X-A-Bool" Bool, Header "X-An-Int" Int] User)

myHeadfulHandler :: Server MyHeadfulHandler
myHeadfulHandler = pure $ addHeader True $ addHeader 1797 albert

myHeadfulHandlerAPI :: Proxy MyHeadfulHandler
myHeadfulHandlerAPI = Proxy

app8 :: Application
app8 = serve myHeadfulHandlerAPI myHeadfulHandler

-- What if sometimes pass a header:
type MyMaybeHeaderHandler = Capture "withHeader" Bool :> Get '[JSON] (Headers '[Header "X-An-Int" Int] User)

myMaybeHeaderHandler :: Server MyMaybeHeaderHandler
myMaybeHeaderHandler x =
    pure
        $ if x
            then addHeader 1797 albert
            else noHeader isaac

maybeHeaderHandlerAPI :: Proxy MyMaybeHeaderHandler
maybeHeaderHandlerAPI = Proxy

app9 :: Application
app9 = serve maybeHeaderHandlerAPI myMaybeHeaderHandler

-- Serving contents of a directory
-- Use of Raw combinator to mean "plug here any WAI application"
-- serveDirectoryWebApp a function to get a file and directory and serve
type StaticAPI = "static" :> Raw

staticAPI :: Proxy StaticAPI
staticAPI = Proxy

server7 :: Server StaticAPI
server7 = serveDirectoryWebApp "static-files"

app10 :: Application
app10 = serve staticAPI server7

-- Nested apis to avoid repetition
type UserAPI3 =
    Capture "userid" Int :> Get '[JSON] User
        :<|> Capture "userid" Int :> DeleteNoContent

-- Like an algebra equation, factor out the Captuer
type UserAPI4 =
    Capture "userid" Int
        :> ( Get '[JSON] User
                :<|> DeleteNoContent
           )

-- notice the difference in server implementation
server8 :: Server UserAPI3
server8 = getUser :<|> deleteUser
  where
    getUser :: Int -> Handler User
    getUser _userid = error "..."

    deleteUser :: Int -> Handler NoContent
    deleteUser _userid = error "..."

server9 :: Server UserAPI4
server9 userid = getUser userid :<|> deleteUser userid
  where
    getUser :: Int -> Handler User
    getUser = error "..."

    deleteUser :: Int -> Handler NoContent
    deleteUser = error "..."

-- More factoring out apis (nested apis)
-- factor out path
type API1 =
    "users"
        :> ( Get '[JSON] [User]
                :<|> Capture "userid" Int :> Get '[JSON] User
           )

-- factor out a reqbody
type API2 =
    ReqBody '[JSON] User
        :> ( Get '[JSON] User
                :<|> PostNoContent
           )

-- factor out a header
type API3 =
    Header "Authorization" Token
        :> ( Get '[JSON] SecretData
                :<|> ReqBody '[JSON] SecretData :> PostNoContent
           )

newtype Token = Token ByteString
newtype SecretData = SecretData ByteString

-- Define APIs modularly and assemble them in one big API
-- Exercise: Implement this server
type UsersAPI =
    Get '[JSON] [User]
        :<|> ReqBody '[JSON] User :> PostNoContent
        :<|> Capture "userid" Int
            :> ( Get '[JSON] User
                    :<|> ReqBody '[JSON] User :> PutNoContent
                    :<|> DeleteNoContent
               )

usersServer :: Server UsersAPI
usersServer = getUsers :<|> newUser :<|> userOperations
  where
    getUsers :: Handler [User]
    getUsers = pure (users1 <> users2)

    newUser :: User -> Handler NoContent
    newUser u = do
        let users = u : (users1 <> users2)
        _ <- liftIO $ mapM_ print users
        pure NoContent

    userOperations userid = viewUser userid :<|> updateUser userid :<|> deleteUser userid
      where
        viewUser :: Int -> Handler User
        viewUser uid = pure $ (users1 <> users2) !! uid

        updateUser :: Int -> User -> Handler NoContent
        updateUser uid user = do
            let users = Data.List.take uid (users1 <> users2) ++ [user] ++ drop uid (users1 <> users2)
            _ <- liftIO $ mapM_ print users
            pure NoContent

        deleteUser :: Int -> Handler NoContent
        deleteUser uid = do
            let users = Data.List.take uid (users1 <> users2) ++ drop uid (users1 <> users2)
            _ <- liftIO $ mapM_ print users
            pure NoContent

type ProductsAPI =
    Get '[JSON] [Product]
        :<|> ReqBody '[JSON] Product :> PostNoContent
        :<|> Capture "productid" Int
            :> ( Get '[JSON] Product
                    :<|> ReqBody '[JSON] Product :> PutNoContent
                    :<|> DeleteNoContent
               )

newtype Product = Product {productId :: Int}
    deriving (Generic, Show, Eq)

instance ToJSON Product
instance FromJSON Product

products :: [Product]
products = [Product 1, Product 2, Product 3]

productsServer :: Server ProductsAPI
productsServer = getProducts :<|> newProduct :<|> productOperations
  where
    getProducts :: Handler [Product]
    getProducts = pure products

    newProduct :: Product -> Handler NoContent
    newProduct p = do
        _ <- liftIO $ mapM_ print (p : products)
        pure NoContent

    productOperations productid = viewProduct productid :<|> updateProduct productid :<|> deleteProduct productid
      where
        viewProduct :: Int -> Handler Product
        viewProduct pid = do
            let p = find ((== pid) . productId) products
            case p of
                Nothing -> throwError (err404{errBody = "Product doesn't exist"})
                Just p' -> pure p'

        updateProduct :: Int -> Product -> Handler NoContent
        updateProduct pid p = do
            let products' = [if productId x == pid then p else x | x <- products]
            _ <- liftIO $ mapM_ print products'
            pure NoContent

        deleteProduct :: Int -> Handler NoContent
        deleteProduct pid = do
            let products' = filter ((/= pid) . productId) products
            _ <- liftIO $ mapM_ print products'
            pure NoContent

type CombinedAPI =
    "users" :> UsersAPI
        :<|> "products" :> ProductsAPI

server10 :: Server CombinedAPI
server10 = usersServer :<|> productsServer

combinedAPI :: Proxy CombinedAPI
combinedAPI = Proxy

app11 :: Application
app11 = serve combinedAPI server10

-- Realize that UsersAPI and ProductsAPI are similar
type APIFor a i =
    Get '[JSON] [a]
        :<|> ReqBody '[JSON] a :> PostNoContent
        :<|> Capture "id" i
            :> ( Get '[JSON] a
                    :<|> ReqBody '[JSON] a :> PutNoContent
                    :<|> DeleteNoContent
               )

serverFor ::
    Handler [a] ->
    (a -> Handler NoContent) ->
    (i -> Handler a) ->
    (i -> a -> Handler NoContent) ->
    (i -> Handler NoContent) ->
    Server (APIFor a i)
serverFor = error "..."

-- Use emptyServer when using EmptyAPI combinator
type CombinedAPI2 = API :<|> "empty" :> EmptyAPI

server11 :: Server CombinedAPI2
server11 = server3 :<|> emptyServer

-- Using another monad
-- Server is Server api = ServerT api Handler
-- which using another monad our endpoint becomes:
-- ServerT (Get '[JSON] Person) SomeMonad
-- and the result would be: SomeMonad Person
readerToHandler :: Reader String a -> Handler a
readerToHandler r = pure (runReader r "hi")

type ReaderAPI =
    "a" :> Get '[JSON] Int
        :<|> "b" :> ReqBody '[JSON] Double :> Get '[JSON] Bool

readerAPI :: Proxy ReaderAPI
readerAPI = Proxy

readerServerT :: ServerT ReaderAPI (Reader String)
readerServerT = a :<|> b
  where
    a :: Reader String Int
    a = pure 1797

    b :: Double -> Reader String Bool
    b _ = asks (== "hi")

-- we cannot run app = serve readerAPI readerServerT
-- we have to use hoistServer
readerServer :: Server ReaderAPI
readerServer = hoistServer readerAPI readerToHandler readerServerT

app12 :: Application
app12 = serve readerAPI readerServer

-- Rewrite Reader Server as an arrow
funServerT :: ServerT ReaderAPI ((->) String)
funServerT = a :<|> b
  where
    a :: String -> Int
    a _ = 1797

    b :: Double -> String -> Bool
    b _ s = s == "hi"

funToHandler :: (String -> a) -> Handler a
funToHandler f = pure (f "hi")

app13 :: Application
app13 = serve readerAPI (hoistServer readerAPI funToHandler funServerT)
