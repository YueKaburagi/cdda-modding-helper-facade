
module Main.Data.Actions where

import Prelude

import Data.Either (Either (..))
import Data.Lens (Prism', prism, Setter')
import Data.Tuple (Tuple(..), uncurry)

import Main.Data.States (CMHFState)



data InfoItemAction
  = ItemQuery String
  | ListQuery (Array String)

data BrowserAction
  = ItemAction Int InfoItemAction
  | ChangeQuery String


_InfoItemAction :: Prism' BrowserAction (Tuple Int InfoItemAction)
_InfoItemAction = prism (uncurry ItemAction) \iia ->
  case iia of
    ItemAction i a -> Right (Tuple i a)
    _ -> Left iia

-- | for ui component
-- | state は一番外の state を入れる
data UIAction state
  = PartialPaddlePos (Setter' state Int) Int
  | Nyoki (Setter' state Boolean) Boolean

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
