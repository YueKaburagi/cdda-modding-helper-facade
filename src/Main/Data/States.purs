
module Main.Data.States where

import Prelude
import Control.Monad.Eff.Exception (Error, error)

import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.List (List(Nil), (:))
import Data.Either (Either (..))

import Util

import Data.Lens (Lens', lens)
import Data.Argonaut.Core (Json, foldJsonObject, JObject, foldJsonArray)
import Data.Argonaut.Core (toString, toObject) as Json
import Data.StrMap (lookup)
import Data.Array (foldl) as Array

import Node.ChildProcess (ChildProcess)


type SymCol =
  { symbol :: String
  , color :: String }
mkSymCol :: String -> String -> SymCol
mkSymCol s c = {symbol: s, color: c}

type InfoItem =
  { symcol :: Maybe SymCol
  , name :: String
  , index :: String }
mkInfoItem :: Maybe SymCol -> String -> String -> InfoItem
mkInfoItem sc n i = {symcol: sc, name: n, index: i}

mockInfoItem :: String -> InfoItem
mockInfoItem n = { symcol: Nothing, name: n, index: "10021" }

jsonToInfoItem :: Json -> Either Error InfoItem
jsonToInfoItem json =
  foldJsonObject
    (Left $ error ("unexpected json value" <> show json)) 
    fromJO
    json
  where
    fromJO :: JObject -> Either Error InfoItem
    fromJO jo = do
      n <- mtoe (error "no name") $ (Json.toString =<< lookup "name" jo)
      i <- mtoe (error "no index") $ (Json.toString =<< lookup "ix" jo)
      pure $ mkInfoItem sc n i
      where
        sc = scFromJO jo
    scFromJO :: JObject-> Maybe SymCol
    scFromJO jo = do
      s <- Json.toString =<< lookup "symbol" jo -- 向きによって変化するシンボルは未対応
      pure $ mkSymCol s c
      where
        c = maybe "undefined" id $ (Json.toString =<< lookup "color" jo)

jsonToListInfoItem :: Json -> Either Error (List InfoItem)
jsonToListInfoItem json =
  foldJsonArray
    (Left $ error ("unexpected json value" <> show json))
    (\ja -> pure $ Array.foldl fl Nil (jsonToInfoItem <$> ja))
    json
  where
    fl :: List InfoItem -> Either Error InfoItem -> List InfoItem -- 下で発生したエラーを握り潰してるのよくない
    fl l (Left _) = l
    fl l (Right r) = r : l
      

type HelperResult =
  { results :: List InfoItem
  , focus :: Maybe Json
  , raw :: Maybe String
  }

initialHelperResult :: HelperResult
initialHelperResult = { results: Nil, focus: Nothing, raw: Nothing }

_results :: Lens' HelperResult (List InfoItem)
_results = lens _.results (_ {results = _ })
_focus :: Lens' HelperResult (Maybe Json)
_focus = lens _.focus (_ {focus = _})
_raw :: Lens' HelperResult (Maybe String)
_raw = lens _.raw (_ {raw = _ })


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
  , queryString :: Array String
  , process :: Maybe ChildProcess
  }
initialCMHFState :: BrowserLayout -> HelperResult -> CMHFState
initialCMHFState bl hr = { layout: bl, result: hr, queryString: [], process: Nothing }

_BrowserLayout :: Lens' CMHFState BrowserLayout
_BrowserLayout = lens _.layout (_ {layout = _})
_HelperResult :: Lens' CMHFState HelperResult
_HelperResult = lens _.result (_ {result = _})
_queryString :: Lens' CMHFState (Array String)
_queryString = lens _.queryString (_ {queryString = _})

-- どっかから Error が raise されてくるはず
-- MasterState に 持たせる？
