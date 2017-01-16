
module Main.Data.Actions where

import Prelude

import Data.Either (Either (..))
import Data.Lens (Prism', prism)
import Data.Tuple (Tuple(..), uncurry)




data InfoItemAction
  = EnterInfo

data BrowserAction
  = ItemAction Int InfoItemAction
  | SendQuery String


_InfoItemAction :: Prism' BrowserAction (Tuple Int InfoItemAction)
_InfoItemAction = prism (uncurry ItemAction) \iia ->
  case iia of
    ItemAction i a -> Right (Tuple i a)
    _ -> Left iia


data UIAction
  = PartialPaddlePos Int

data CMHFAction
  = BrAct BrowserAction
  | UIAct UIAction

_BrowserAction :: Prism' CMHFAction BrowserAction
_BrowserAction = prism BrAct \a ->
  case a of
    BrAct hr -> Right hr
    _ -> Left a

_UIAction :: Prism' CMHFAction UIAction
_UIAction = prism UIAct \a ->
  case a of
    UIAct uia -> Right uia
    _ -> Left a
