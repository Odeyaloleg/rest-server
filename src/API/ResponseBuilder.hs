{-# LANGUAGE OverloadedStrings #-}

module API.ResponseBuilder where

import Data.Aeson (ToJSON(toJSON), encode, object, (.=))
import Data.ByteString.Lazy (ByteString)
import qualified API.Entities.Users as Users
import qualified API.Entities.Tags as Tags
import qualified API.Entities.Categories as Categories

data APIResponse
  = UsersListBuilder [Users.User]
  | CreateUserBuilder Users.User
  | DeleteUserBuilder
  | TagsListBuilder [Tags.Tag]
  | CreateTagBuilder Tags.Tag
  | DeleteTagBuilder
  | EditTagBuilder
  | CreateCategoryBuilder Categories.Category -- unsafe for now
  | BadRequestBuilder String

instance ToJSON APIResponse where
  toJSON (UsersListBuilder users) = object ["success" .= True, "users" .= users]
  toJSON (CreateUserBuilder user) = object ["success" .= True, "user" .= user]
  toJSON DeleteUserBuilder = object ["success" .= True]
  toJSON (TagsListBuilder tags) = object ["success" .= True, "tags" .= tags]
  toJSON (CreateTagBuilder tag) = object ["success" .= True, "tag" .= tag]
  toJSON DeleteTagBuilder = object ["success" .= True]
  toJSON EditTagBuilder = object ["success" .= True]
  toJSON (CreateCategoryBuilder category) = object ["success" .= True, "category" .= category]
  toJSON (BadRequestBuilder description) =
    object ["success" .= False, "bad_request" .= description]

buildResponse :: APIResponse -> ByteString
buildResponse = encode
