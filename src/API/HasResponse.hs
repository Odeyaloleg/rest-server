module API.HasResponse where

import qualified API.Entities.Users as Users

data AccessLevel
  = AccessUser
  | AccessAuthor Int
  | AccessAdmin
  deriving (Eq)

data Request
  = UsersList
      { pageNum :: Int
      }
  | CreateUser Users.User
  | BadRequest String

class HasResponse a where
  getResponse :: Request -> a
