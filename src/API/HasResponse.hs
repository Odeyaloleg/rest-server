module API.HasResponse where

import qualified API.Entities.Tags as Tags
import qualified API.Entities.Users as Users
import Models.User (UserId, FirstName, LastName, ProfilePicture)
import Models.Tag (TagId)
import Models.Author (AuthorDescription)
import Models.Category (CategoryId, ParentCategoryId)
import Models.Post (PostId, Title, MainPicture, Content, AdditionalPicture)
import Models.Comment (CommentId)
import Models.Draft (DraftId)

type PageNum = Int

data AccessLevel
  = AccessUser UserId
  | AccessAuthor UserId
  | AccessAdmin UserId
  deriving (Eq)

data Request
  = UsersList PageNum
  | CreateUser FirstName LastName ProfilePicture
  | CreateAdmin FirstName LastName ProfilePicture
  | DeleteUser UserId
  | TagsList PageNum
  | CreateTag String
  | DeleteTag TagId
  | EditTag TagId String
  | AuthorsList PageNum
  | CreateAuthor UserId AuthorDescription
  | DeleteAuthor UserId
  | EditAuthor UserId AuthorDescription
  | CategoriesList PageNum
  | CreateCategory (Maybe ParentCategoryId) String
  | DeleteCategory CategoryId
  | EditCategory CategoryId (Maybe ParentCategoryId) String
  | CommentsList PostId PageNum
  | PublishComment PostId String
  | DeleteComment CommentId
  | DraftsList PageNum
  | PublishDraft DraftId
  | CreateDraft
      (Maybe PostId)
      (Maybe CategoryId)
      (Maybe Title)
      (Maybe [TagId])
      (Maybe MainPicture)
      (Maybe Content)
      (Maybe [AdditionalPicture])
  | DeleteDraft DraftId
  | EditDraft
      DraftId
      (Maybe CategoryId)
      (Maybe Title)
      (Maybe [TagId])
      (Maybe MainPicture)
      (Maybe Content)
      (Maybe [AdditionalPicture])
  | PostsList PageNum
  | PublishPost
      CategoryId
      Title
      (Maybe [TagId])
      (Maybe Content)
      (Maybe MainPicture)
      (Maybe [AdditionalPicture])
  | DeletePost PostId
  | BadRequest String

class HasResponse a where
  getResponse :: Request -> a

withAdminAccess :: (HasResponse a) => AccessLevel -> a -> a
withAdminAccess access request =
  case access of
    AccessAdmin _ -> request
    _ -> wrongPath

withAuthorAccess :: (HasResponse a) => AccessLevel -> a -> a
withAuthorAccess access request =
  case access of
    AccessAdmin _ -> request
    _ -> notAuthor

wrongJSON :: (HasResponse a) => a
wrongJSON = getResponse $ BadRequest "Wrong\\insufficient JSON data."

wrongPath :: (HasResponse a) => a
wrongPath = getResponse $ BadRequest "Resource path does not exist."

notAuthor :: (HasResponse a) => a
notAuthor = getResponse $ BadRequest "No rights provided. Needed to be an author."
