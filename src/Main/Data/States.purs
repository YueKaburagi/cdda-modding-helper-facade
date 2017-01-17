
module Main.Data.States where

import Prelude

import Data.Maybe (Maybe(Just, Nothing))
import Data.List (List(Nil), (:))
import Data.Either (Either (..))

import Util

import Data.Lens (Lens', lens)
import Data.Argonaut.Core (Json)

type SymCol =
  { symbol :: String
  , color :: String }

type InfoItem =
  { symcol :: Maybe SymCol
  , name :: String
  , index :: String }

mockInfoItem :: String -> InfoItem
mockInfoItem n = { symcol: Nothing, name: n, index: "#undefined" }


type HelperResult =
  { results :: List InfoItem
  , focus :: Maybe Json
  , raw :: Maybe String
  }

initialHelperResult :: HelperResult
initialHelperResult = { results: Nil, focus: Nothing, raw: Nothing }

_results :: Lens' HelperResult (List InfoItem)
_results = lens _.results (_ {results = _ })


type BrowserLayout =
  { resultPaneWidth :: Int
  , itemInfoHeight :: Int
  }
initialBrowserLayout :: BrowserLayout
initialBrowserLayout = { resultPaneWidth: 100, itemInfoHeight: 100 }

_resultPaneWidth :: Lens' BrowserLayout Int
_resultPaneWidth = lens _.resultPaneWidth (_ {resultPaneWidth = _})
_itemInfoHeight :: Lens' BrowserLayout Int
_itemInfoHeight = lens _.itemInfoHeight (_ {itemInfoHeight = _})
-- x bound: 10px (document.width - 10px)
-- y bound:

type CMHFState =
  { layout :: BrowserLayout
  , result :: HelperResult
  }
initialCMHFState :: BrowserLayout -> HelperResult -> CMHFState
initialCMHFState bl hr = { layout: bl, result: hr }

_BrowserLayout :: Lens' CMHFState BrowserLayout
_BrowserLayout = lens _.layout (_ {layout = _})
_HelperResult :: Lens' CMHFState HelperResult
_HelperResult = lens _.result (_ {result = _})

-- どっかから Error が raise されてくるはず
-- MasterState に 持たせる？
