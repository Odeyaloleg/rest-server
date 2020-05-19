module Models.Post where

import Models.User (UserId)

type AuthorId = UserId

type PostId = Int

type Title = String

type Content = String

type MainPicture = String

type AdditionalPicture = String
