{-# LANGUAGE OverloadedStrings #-}

module API
  ( QueryData(..)
  , runAPI
  ) where

import qualified API.Entities.Users as Users
import qualified API.Methods.Users as Users.M
import API.Methods.Generic (badRequest)
import qualified API.HasResponse as R
import Data.Aeson (decode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Text.Read (readMaybe)

data QueryData =
  QueryData
    { method :: T.Text
    , params :: [(BS.ByteString, (Maybe BS.ByteString))]
    , body :: BSL.ByteString
    }

runAPI :: (R.HasResponse a) => QueryData -> R.AccessLevel -> a
runAPI queryData access =
  case method queryData of
    "users" ->
      case lookup "page" (params queryData) of
        Just pageNum ->
          maybe
            (badRequest "Bad path parameters.")
            Users.M.getUsers
            (maybe Nothing (readMaybe . BSC.unpack) pageNum)
        Nothing -> Users.M.getUsers 1
    "createUser" -> Users.M.createUser 3 (decode (body queryData) :: Maybe Users.UserCreation)
    "createAdmin" ->
      Users.M.createAdmin 3 (decode (body queryData) :: Maybe Users.AdminCreation) access
    _ -> badRequest "Resource path does not exist."
