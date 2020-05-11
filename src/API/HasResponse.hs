module API.HasResponse where

import qualified API.Entities.Users as Users
import qualified API.Entities.Tags as Tags
import qualified Types as T

data AccessLevel
  = AccessUser Int
  | AccessAuthor Int
  | AccessAdmin Int
  deriving (Eq)

data Request
  = UsersList T.PageNum
  | CreateUser T.FirstName T.LastName T.ProfilePicture 
  | CreateAdmin T.FirstName T.LastName T.ProfilePicture 
  | DeleteUser T.Id
  | TagsList T.PageNum
  | CreateTag String
  | DeleteTag T.Id
  | EditTag T.Id String
  | BadRequest String

class HasResponse a where
  getResponse :: Request -> a

withAdminAccess :: (HasResponse a) => AccessLevel -> a -> a
withAdminAccess access request =
  case access of
    AccessAdmin _ -> request
    _ -> wrongPath

wrongJSON :: (HasResponse a) => a
wrongJSON = getResponse $ BadRequest "Wrong\\insufficient JSON data."

wrongPath :: (HasResponse a) => a
wrongPath = getResponse $ BadRequest "Resource path does not exist."
