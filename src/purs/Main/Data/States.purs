
module Main.Data.States where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.State (State, StateT)

import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.List (List(Nil), (:))
import Data.Either (Either (..))

import Util
import Main.Data.Query (Query)

import Data.Lens (Lens', lens)
import Data.Argonaut.Core (Json, foldJsonObject, JObject, foldJsonArray, jsonNull)
import Data.Argonaut.Core (toString, toObject) as Json
import Data.StrMap (lookup)
import Data.Array (foldl) as Array
import Data.Int (fromString) as Int

import Node.ChildProcess (ChildProcess)


type SymCol =
  { symbol :: String
  , color :: String
  , bgcolor :: Maybe String } 
mkSymCol :: String -> String -> SymCol
mkSymCol s c = {symbol: s, color: c, bgcolor: Nothing }

data ClickActionType
  = CATIndex String
  | CATQuery (Array Query)

type InfoItem =
  { symcol :: Maybe SymCol
  , name :: String
  , evnType :: ClickActionType
  }
mkInfoItem :: Maybe SymCol -> String -> ClickActionType -> InfoItem
mkInfoItem sc n t = {symcol: sc, name: n, evnType: t}

mockInfoItem :: String -> InfoItem
mockInfoItem n = { symcol: Nothing, name: n, evnType: CATIndex "10021"}

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
      pure $ mkInfoItem sc n (CATIndex i)
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
  , focus :: Json
  , raw :: Maybe String
  }

initialHelperResult :: HelperResult
initialHelperResult = { results: Nil, focus: jsonNull, raw: Nothing }

_results :: Lens' HelperResult (List InfoItem)
_results = lens _.results (_ {results = _ })
_focus :: Lens' HelperResult Json
_focus = lens _.focus (_ {focus = _})
_raw :: Lens' HelperResult (Maybe String)
_raw = lens _.raw (_ {raw = _ })



type Prompt eff = StateT OuterState (Aff eff)
type OuterState =
  { ready :: Boolean
  , process :: ChildProcess
  , busy :: Boolean
  }
initialOuterState :: ChildProcess -> OuterState
initialOuterState p = {ready: false, process: p, busy: false}

_ready :: Lens' OuterState Boolean
_ready = lens _.ready (_ {ready = _})
-- _process :: Getter' OuterState ChildProcess
_busy :: Lens' OuterState Boolean
_busy = lens _.busy (_ {busy =_})


type QueryHelperState =
  { sortDesc :: Boolean
  , sortTarget :: String
  , upto :: Int
  , filterMod :: String
  , key :: String
  , value :: String
  , nyokking :: Boolean
  }
initialQueryHelperState =
  { sortDesc: false
  , sortTarget: ""
  , upto: 25
  , filterMod: ""
  , key: ""
  , value: ""
  , nyokking: false
  }
_sortDesc :: Lens' QueryHelperState Boolean
_sortDesc = lens _.sortDesc (_ {sortDesc = _})
_sortTarget :: Lens' QueryHelperState String
_sortTarget = lens _.sortTarget (_ {sortTarget = _})
_upto :: Lens' QueryHelperState String
_upto = lens (\s -> intToString s.upto) (\s a ->
                                    case Int.fromString a of
                                      Just n -> s {upto = n}
                                      Nothing -> s
                                  )
_filterMod :: Lens' QueryHelperState String
_filterMod = lens _.filterMod (_ {filterMod = _})
_key :: Lens' QueryHelperState String
_key = lens _.key (_ {key = _})
_value :: Lens' QueryHelperState String
_value = lens _.value (_ {value = _})
_nyokking :: Lens' QueryHelperState Boolean
_nyokking = lens _.nyokking (_ {nyokking = _})

type BrowserLayout =
  { resultPaneWidth :: Int
  , itemInfoHeight :: Int
  }
initialBrowserLayout :: BrowserLayout
initialBrowserLayout = { resultPaneWidth: 180, itemInfoHeight: 220 }

_resultPaneWidth :: Lens' BrowserLayout Int
_resultPaneWidth = lens _.resultPaneWidth (_ {resultPaneWidth = _})
_itemInfoHeight :: Lens' BrowserLayout Int
_itemInfoHeight = lens _.itemInfoHeight (_ {itemInfoHeight = _})

type UIState =
  { layout :: BrowserLayout
  , queryString :: String
  , qhs :: QueryHelperState
  }
initialUIState = {
  layout: initialBrowserLayout,
  queryString: "",
  qhs: initialQueryHelperState
  }

_BrowserLayout :: Lens' UIState BrowserLayout
_BrowserLayout = lens _.layout (_ {layout = _})
_queryString :: Lens' UIState String
_queryString = lens _.queryString (_ {queryString = _})
_QueryHelperState :: Lens' UIState QueryHelperState
_QueryHelperState = lens _.qhs (_ {qhs = _})

type CMHFState =
  { result :: HelperResult
  , queries :: Array Query
  , ui :: UIState
  , outer :: OuterState
  }
initialCMHFState :: HelperResult -> ChildProcess -> CMHFState
initialCMHFState hr cp = { result: hr, queries: [], ui: initialUIState, outer: o }
  where
    o = initialOuterState cp

_HelperResult :: Lens' CMHFState HelperResult
_HelperResult = lens _.result (_ {result = _})
_queries :: Lens' CMHFState (Array Query)
_queries = lens _.queries (_ {queries = _})
_UIState :: Lens' CMHFState UIState
_UIState = lens _.ui (_ {ui = _})
_OuterState :: Lens' CMHFState OuterState
_OuterState = lens _.outer (_ {outer = _})

-- どっかから Error が raise されてくるはず
-- MasterState に 持たせる？
