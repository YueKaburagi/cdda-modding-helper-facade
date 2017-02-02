
module Main.Data.Actions where

import Prelude

import Data.Either (Either (..))
import Data.Lens (Prism', prism, Setter')
import Data.Tuple (Tuple(..), uncurry)

import Main.Data.States (CMHFState)
import Main.Data.Query


data BrowserAction
  = ItemQuery String
  | SendQuery (Array String)
  | SendQueryString -- | SendQuery state.queries
  | SetQuery (Array Query)
  | AddQuery (Array Query)
  | FlushQuery -- | flush state.queries to ui.queryString
  | RemoveQuery Query
  | ChangeQuery String


-- | for ui component
-- | state は一番外の state を入れる
data UIAction state
  = PartialPaddlePos (Setter' state Int) Int
  | UpdateBool (Setter' state Boolean) Boolean
  | UpdateString (Setter' state String) String


data CMHFAction
  = BrAct BrowserAction
  | UIAct (UIAction CMHFState) 

_BrowserAction :: Prism' CMHFAction BrowserAction
_BrowserAction = prism BrAct \a ->
  case a of
    BrAct hr -> Right hr
    _ -> Left a
_UIAction :: Prism' CMHFAction (UIAction CMHFState)
_UIAction = prism UIAct \a ->
  case a of
    UIAct uia -> Right uia
    _ -> Left a
