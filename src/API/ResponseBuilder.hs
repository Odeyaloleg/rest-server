{-# LANGUAGE OverloadedStrings #-}

module API.ResponseBuilder where

import Data.Aeson (ToJSON(toJSON), encode, object, (.=))
import Data.ByteString.Lazy (ByteString)
import qualified API.Entities.Users as Users

data APIResponse
  = UsersListBuilder [Users.User]
  | CreateUserBuilder Users.User
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

buildResponse :: APIResponse -> ByteString
buildResponse = encode
