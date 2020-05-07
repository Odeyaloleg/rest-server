{-# LANGUAGE OverloadedStrings #-}

module API
  ( AccessLevel(..)
  , Request(..)
  , APIResponse(..)
  , HasResponse(..)
  , QueryData(..)
  , runAPI
  , buildResponse
  ) where

import API.Entities
import Data.Aeson (ToJSON(toJSON), (.=), decode, encode, object)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as MS
import qualified Data.Text as T
import Text.Read (readMaybe)

data QueryData =
  QueryData
    { method :: T.Text
    , params :: [(BS.ByteString, (Maybe BS.ByteString))]
    , body :: BSL.ByteString
    }

data AccessLevel
  = AccessUser
  | AccessAuthor Int
  | AccessAdmin
  deriving (Eq)

data Request
  = UsersList
      { pageNum :: Int
      }
  | CreateUser User
  | NewsList
  | NewsItem Int
  | BadRequest String

data APIResponse
  = UsersListBuilder [User]
  | CreateUserBuilder User
  | NewsBuilder String
  | NewsItemBuilder String
  | BadRequestBuilder String

instance ToJSON APIResponse where
  toJSON (UsersListBuilder users) = object ["success" .= True, "users" .= users]
  toJSON (CreateUserBuilder user) = object ["success" .= True, "user" .= user]
  toJSON (NewsBuilder text) = object ["success" .= True, "news_list" .= text]
  toJSON (NewsItemBuilder text) =
    object ["success" .= True, "news_item" .= text]
  toJSON (BadRequestBuilder description) =
    object ["success" .= False, "bad_request" .= description]

class HasResponse a where
  getResponse :: Request -> a

getUsers :: (HasResponse a) => Int -> a
getUsers pageNum = getResponse $ UsersList pageNum

createUser :: (HasResponse a) => Int -> Maybe UserCreation -> a
createUser uId userMaybe =
  case userMaybe of
    Just user ->
      getResponse $
      CreateUser $
      User
        uId
        (firstNameUser user)
        (lastNameUser user)
        (profilePictureUser user)
        ("Today")
        False
    Nothing -> getResponse $ BadRequest "Wrong\\insufficient JSON data."

createAdmin :: (HasResponse a) => Int -> Maybe AdminCreation -> AccessLevel -> a
createAdmin uId adminMaybe access =
  if access == AccessAdmin
    then case adminMaybe of
           Just admin ->
             getResponse $
             CreateUser $
             User
               uId
               (firstNameAdmin admin)
               (lastNameAdmin admin)
               (profilePictureAdmin admin)
               ("Today")
               True
           Nothing -> getResponse $ BadRequest "Wrong\\insufficient JSON data."
    else getResponse $ BadRequest "Resource path does not exist."

getNews :: (HasResponse a) => a
getNews = getResponse NewsList

loadNewsItem :: (HasResponse a) => Int -> a
loadNewsItem newsId = getResponse $ NewsItem newsId

badRequest :: (HasResponse a) => String -> a
badRequest description = getResponse $ BadRequest description

runAPI :: (HasResponse a) => QueryData -> AccessLevel -> a
runAPI queryData access =
  case method queryData of
    "users" ->
      case lookup "page" (params queryData) of
        Just pageNum ->
          maybe
            (badRequest "Bad path parameters.")
            getUsers
            (maybe Nothing (readMaybe . BSC.unpack) pageNum)
        Nothing -> getUsers 1
    "createUser" -> createUser 3 (decode (body queryData) :: Maybe UserCreation)
    "createAdmin" ->
      createAdmin 3 (decode (body queryData) :: Maybe AdminCreation) access
    "getNews" -> getNews
    "loadNewsItem" -> loadNewsItem 1
    _ -> badRequest "Resource path does not exist."

buildResponse :: APIResponse -> BSL.ByteString
buildResponse = encode
