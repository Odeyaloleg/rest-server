{-# LANGUAGE OverloadedStrings #-}

module API
  ( QueryData(..)
  , runAPI
  ) where

import qualified API.Entities.Users as Users
import qualified API.Entities.Tags as Tags
import qualified API.Entities.Authors as Authors
import qualified API.Entities.Categories as Categories
import qualified API.Entities.Comments as Comments
import qualified API.HasResponse as R
import API.Methods.Generic (badRequest)
import qualified API.Methods.Users as Users.M
import qualified API.Methods.Tags as Tags.M
import qualified API.Methods.Authors as Authors.M
import qualified API.Methods.Categories as Categories.M
import qualified API.Methods.Comments as Comments.M
import Data.Aeson (decode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Text.Read (readMaybe)

data QueryData =
  QueryData
    { requestMethod :: BS.ByteString
    , apiMethod :: T.Text
    , params :: [(BS.ByteString, (Maybe BS.ByteString))]
    , body :: BSL.ByteString
    }

runAPI :: (R.HasResponse a) => QueryData -> R.AccessLevel -> a
runAPI queryData access =
  case requestMethod queryData of
    "GET" ->
      case apiMethod queryData of
        "users" ->
          case lookup "page" (params queryData) of
            Just pageNum ->
              maybe
                (badRequest "Bad path parameters.")
                Users.M.getUsers
                (maybe Nothing (readMaybe . BSC.unpack) pageNum)
            Nothing -> Users.M.getUsers 1
        "tags" ->
          case lookup "page" (params queryData) of
            Just pageNum ->
              maybe
                (badRequest "Bad path parameters.")
                Tags.M.getTags
                (maybe Nothing (readMaybe . BSC.unpack) pageNum)
            Nothing -> Tags.M.getTags 1
        "authors" ->
          case lookup "page" (params queryData) of
            Just pageNum ->
              maybe
                (badRequest "Bad path parameters.")
                (Authors.M.getAuthors access)
                (maybe Nothing (readMaybe . BSC.unpack) pageNum)
            Nothing -> Tags.M.getTags 1
        "categories" ->
          case lookup "page" (params queryData) of
            Just pageNum ->
              maybe
                (badRequest "Bad path parameters.")
                Categories.M.getCategories
                (maybe Nothing (readMaybe . BSC.unpack) pageNum)
            Nothing -> Tags.M.getTags 1
        _ -> badRequest "Resource path does not exist."
    "POST" ->
      case apiMethod queryData of
        "createUser" ->
          Users.M.createUser
            (decode (body queryData) :: Maybe Users.UserCreation)
        "createAdmin" ->
          Users.M.createAdmin
            (decode (body queryData) :: Maybe Users.AdminCreation)
            access
        "createTag" ->
          Tags.M.createTag
            (decode (body queryData) :: Maybe Tags.TagCreation)
            access
        "deleteTag" ->
          Tags.M.deleteTag
            (decode (body queryData) :: Maybe Tags.TagDeletion)
            access
        "editTag" ->
          Tags.M.editTag
            (decode (body queryData) :: Maybe Tags.TagEditing)
            access
        "createAuthor" ->
          Authors.M.createAuthor
            access
            (decode (body queryData) :: Maybe Authors.AuthorCreation)
        "deleteAuthor" ->
          Authors.M.deleteAuthor
            access
            (decode (body queryData) :: Maybe Authors.AuthorDeletion)
        "editAuthor" ->
          Authors.M.editAuthor
            access
            (decode (body queryData) :: Maybe Authors.AuthorEditing)
        "createCategory" ->
          Categories.M.createCategory
            access
            (decode (body queryData) :: Maybe Categories.CategoryCreation)
        "deleteCategory" ->
          Categories.M.deleteCategory
            access
            (decode (body queryData) :: Maybe Categories.CategoryDeletion)
        "editCategory" ->
          Categories.M.editCategory
            access
            (decode (body queryData) :: Maybe Categories.CategoryEditing)
        _ -> badRequest "Resource path does not exist."
    _ -> badRequest "Bad request method. Use GET or POST."
