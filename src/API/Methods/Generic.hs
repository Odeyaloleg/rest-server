module API.Methods.Generic where

import qualified API.HasResponse as R

badRequest :: (R.HasResponse a) => String -> a
badRequest description = R.getResponse $ R.BadRequest description
