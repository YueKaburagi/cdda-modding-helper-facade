
module Main.Data.Query where

import Prelude ((==))
import Data.Eq (class Eq)
import Data.Generic (class Generic, gEq)

data QueryMode = Find | Lookup
data QueryNumOfItems = UpTo Int | All
data QuerySort = Asc String | Desc String
data QueryFilter
  = HasField String String
  | HasKey String
  | HasValue String
  | ModIdent String
  | No QueryFilter

data Query
  = Mode QueryMode
  | NumOfItems QueryNumOfItems
  | Sort QuerySort
  | Filter QueryFilter
  | Unknown String
  | UnexpectedError

querySortBy :: QuerySort -> String
querySortBy (Asc  x) = x
querySortBy (Desc x) = x

derive instance genericQMode :: Generic QueryMode
derive instance genericQNumOfItems :: Generic QueryNumOfItems
derive instance genericQSort :: Generic QuerySort
derive instance genericQFilter :: Generic QueryFilter

derive instance genericQuery :: Generic Query

instance eqQueryMode :: Eq QueryMode where  eq = gEq
instance eqQueryFilter :: Eq QueryFilter where  eq = gEq
instance eqQueryNumOfItems :: Eq QueryNumOfItems where  eq = gEq
instance eqQuerySort :: Eq QuerySort where eq = gEq
instance eqQuery :: Eq Query where eq = gEq                                           

