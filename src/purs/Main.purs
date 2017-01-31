module Main (main, dropIv) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff, liftEff')
import Control.Monad.Trans.Class (lift)
import Control.Monad.Eff.Exception (EXCEPTION, Error, error, throwException)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.State.Trans (runStateT)

import Data.Nullable (toMaybe)
import Data.Maybe (Maybe(..), maybe)
import Data.List (List(Cons, Nil), head, (!!))
import Data.List (fromFoldable) as List
import Data.Array as Array
import Data.Either (Either (..))
import Data.Foldable (fold)
import Data.String (split, joinWith) as String
import Data.String.Regex (regex)
import Data.String.Regex (split) as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.Int as Int

import DOM.Node.Types (ElementId (..))
import DOM (DOM)
import DOM.File.Types (File)

import Node.ChildProcess (CHILD_PROCESS, ChildProcess)
import Node.Buffer (BUFFER)

import Main.Data
import Main.Data.Query
import Main.Query (QueryRule(..), QueryDisplayRule(..), queryBuilder)
import Main.Query as Query
import Util
import Outer
import Thermite.MyUtil as TU
import DOM.HTML.MyUtil as HU

import Data.Lens
-- Lens (lens)
-- Prism (prism, Prism' APrism')
-- Getter ((^.))
-- Setter (over, set, Setter')
import Data.Tuple
import Data.StrMap (lookup)
import Thermite as T
import React (ReactElement)
import React.DOM.Props (Props)
import React as R
import React.DOM as R
import React.DOM.Props as P
import ReactDOM as RD
import Data.Argonaut.Core (Json, jsonNull, foldJsonObject, JObject, foldJson, foldJsonNumber)
import Data.Argonaut.Core (toObject, fromNumber, fromString) as Json
import Text.Format as Format

import Unsafe.Coerce (unsafeCoerce)

import Node.OS (Platform(Darwin), OS)
import Node.OS (platform) as OS
import Node.Path (normalize) as Path


specInfoItem :: forall eff props . T.Spec eff InfoItem props InfoItemAction
specInfoItem = T.simpleSpec T.defaultPerformAction render
  where
  render :: T.Render InfoItem props InfoItemAction
  render dispatch _ mi _ =
    [ R.li (col mi.symcol)
      (sym (mi.symcol) <>
       [ R.a
         [ P.onClick \_ -> evn mi.evnType ]
         [ R.text mi.name ]
       ]
      )
    ]
    where
      evn (CATIndex ix) = dispatch $ ItemQuery ix
      evn (CATQuery qs) = dispatch $ SetQuery qs
  col (Just s) = [ P.className s.color ]
  col Nothing  = []
  sym (Just s) = [ R.span' [ R.text s.symbol ] ]
  sym Nothing  = [ R.span' [] ]


specResultPane :: forall eff props . T.Spec eff HelperResult props BrowserAction
specResultPane = ulist $ T.focus _results _InfoItemAction $ T.foreach \_ -> specInfoItem
  where
  ulist :: forall state action. T.Spec eff state props action -> T.Spec eff state props action
  ulist = over T._render \render d p s c ->
    [ R.ul' (render d p s c) ]
  -- foreach しか提供されてなくて、しかもあの形なのはなぜなんだろう？


specContain :: forall eff state props action
               . String
               -> T.Spec eff state props action
               -> T.Spec eff state props action
specContain cls = over T._render \render d p s c ->
  [ R.div [ P.className cls ] (render d p s c) ]

-- ResizablePaneW で包む
specBrowser :: forall eff props . T.Spec _ CMHFState props CMHFAction
specBrowser =
  specContain "browser" (
    mkSpecResizableW (_BrowserLayout <<< _resultPaneWidth) _UIAction 
      (T.focus _HelperResult _BrowserAction specResultPane)
    <>
    specContain "expand-w-tail" (
      mkSpecResizableH (_BrowserLayout <<< _itemInfoHeight) _UIAction
        (T.focusState (_HelperResult <<< _focus) specMayItem)
      <>
      T.focusState _HelperResult specRawJson
    )
  )
  <>
  ardo
  <>
  T.match _BrowserAction (catchBrowserQuery TU.unitSpec)

-- 自動で className つけていいような？
-- action は BrowserAction ？
makeItemInfo :: forall action . (action -> T.EventHandler) -> JObject -> Array ReactElement
makeItemInfo dispatch j =
  mkt'  (Just j) "modname" (R.span [P.className "info-modname"]) "" "" <>
  spn'' b "name" <>
  spn   b "volume" "体積: " " l" (mapjn (_ * 0.25) >>> mapjf (Format.precision 2)) <>
  spn   b "weight" "質量: " " kg" (mapjn (_ * 0.001) >>> mapjf (Format.precision 3)) <>
                                    -- 1[kg] == 2.20462[lbs]
  spn'  b "to_hit" "命中: " "" <>
  spn'  b "bashing" "打撃: " "" <>
  spn'  b "cutting" "切断: " "" <>
  spn'  b "fun" "満喫: " "" <>
  spn'  b "quench" "水分: " "" <>
  spn'  b "nutrition" "満腹: " "" <>
  spn'  b "healthy" "健康: " "" <> -- spoil
  spn'  b "stim" "神経作用: " "" <> -- spoil
  spn'  b "addiction_potential" "依存性: " "" <> -- spoil
  spn'  b "coverage" "被覆率: " " %" <>
  spn'  b "encumbrance" "動作制限: " "" <>
  spn'  b "warmth" "暖かさ: " "" <>
  spn'  b "environmental_protection" "環境防護: " "" <>
  mkt'  b "description" R.p' "" ""
  where
    b = Json.toObject =<< lookup "body" j
    mkTag :: (Array ReactElement -> ReactElement) -- | tag
             -> String -- | prefix
             -> String -- | suffix
             -> Maybe String -- | content
             -> Array ReactElement
    mkTag tag p s (Just x) = [ tag [ R.text (p <> x <> s) ] ]
    mkTag _ _ _ Nothing = []
    mkTag' = mkTag R.span'
    mkProp :: (String -> Props) -- | prop
              -> Maybe String
              -> Array Props
    mkProp p (Just s) = [ p s ]
    mkProp _ Nothing = []
    mapjn :: (Number -> Number) -> Json -> Json
    mapjn f j = foldJsonNumber j (Json.fromNumber <<< f) j
    mapjf :: Format.Properties -> Json -> Json
    mapjf ps j = foldJsonNumber j (Json.fromString <<< Format.format ps) j
    toText :: Json -> String
    toText = foldJson
             (\_ -> "(null)")
             (\b -> show b)
             (\n -> dispNum n)
             (\s -> s)
             (\arr -> show arr)
             (\jo -> show jo)
    dispNum :: Number -> String
    dispNum n =
      case Int.fromNumber n of
        Just i | Int.toNumber i == n -> show i
        _ -> show n
    mkp :: (Maybe JObject) -- | src
           -> String -- | key
           -> (String -> Props) -- | prop
           -> (Json -> Json) -- | json transfotm
           -> Array Props
    mkp  x k p f = mkProp p (toText <$> f <$> (lookup k =<< x))
    mkp' x k p = mkp x k p id
    mkt :: (Maybe JObject) -- | src
           -> String -- | key
           -> (Array ReactElement -> ReactElement) -- | tag
           -> String -- | prefix
           -> String -- | suffix
           -> (Json -> Json) -- | json transform
           -> Array ReactElement
    mkt   x k t p s f = mkTag t p s (toText <$> f <$> (lookup k =<< x))
    mkt'  x k t p s = mkt x k t p s id
    spn   x k = mkt x k R.span'
    spn'  x k p s = spn x k p s id
    spn'' x k = spn' x k "" ""
    

specMayItem :: forall eff props action . T.Spec _ Json props action
specMayItem = T.simpleSpec T.defaultPerformAction render
  where
    render :: T.Render Json props action
    render dispatch _ json _ =
      [ R.div
        [ P.className "json-info" ]
        (foldJsonObject [] (makeItemInfo dispatch) json)
      ]

-- query control
paSearchBar :: forall eff props . T.PerformAction _ CMHFState props BrowserAction
paSearchBar (ChangeQuery qs) _ _ = do
  TU.stateUpdate_ (_UIState <<< _queryString) qs
  ss <- lift <<< liftEff <<< sep $ qs
  TU.stateUpdate_ _queries $ Query.encode qRules ss
    where
      sep :: String -> Eff _ (Array String)
      sep s = do
        r <- esToEff $ regex "\\s+" RegexFlags.global
        pure $ dropEmptyHeads $ Regex.split r s
      dropEmptyHeads :: Array String -> Array String
      dropEmptyHeads arr =
        case Array.uncons arr of -- ここもにょい
          Just {head: "", tail: xs} ->  dropEmptyHeads xs
          _ -> arr
paSearchBar _ _ _ = pure unit


toggled :: forall a . Boolean -> a -> a -> a
toggled true  t _ = t
toggled false _ f = f


ardo :: forall props . T.Spec _ CMHFState props CMHFAction
ardo =
  specSearchArea 
    _b
    (T.match _BrowserAction specSearchBar
     <>
     T.match _UIAction (specNyoki _b))
  where
    _b :: Lens' CMHFState Boolean
    _b = _BrowserLayout <<< _nyokking
    
specSearchArea :: forall eff props
                  . Setter' CMHFState Boolean
                  -> T.Spec _ CMHFState props CMHFAction
                  -> T.Spec _ CMHFState props CMHFAction
specSearchArea _b = over T._render \render dispatch p s c ->
  [ R.div
    [ P.className "search-area"
    , P.onMouseOver \_ -> dispatch $ UIAct $ Nyoki _b true
    , P.onMouseLeave \_ -> dispatch $ UIAct $ Nyoki _b false
    ]
    (render dispatch p s c)
  ]

specSearchBar :: forall eff props . T.Spec _ CMHFState props BrowserAction
specSearchBar = T.simpleSpec paSearchBar render
  where
    render :: T.Render CMHFState props BrowserAction
    render dispatch p s c =
      [ R.button
        [ P.onClick \_ -> dispatch $ ItemAction 0 $ ListQuery (Query.decode qRules s.queries) ]
        [ R.text "=>" ]
      , R.div
        [ P.className "query-indicator" ]
        (((T.focus
            (_UIState <<< _QueryHelperState)
            _InfoItemActionB
            (specArrayQuery s.queries)) ^. T._render) dispatch p s c)
      , R.div -- ここの value と queryString は別々に管理する？
        [ P.className ("search-bar " <> toggled (s.layout.nyokking) "search-bar-hide" "search-bar-show") ]
        [ R.input
          [ P._type "search"
          , P.className "search"
          , P.placeholder " looking ... | find ..."
          , P.value (s ^. (_UIState <<< _queryString) )
          , P.onKeyUp \e -> handleKey (unsafeCoerce e).keyCode
          , P.onChange \e -> dispatch $ ChangeQuery (unsafeCoerce e).target.value
          ]
          []
        ]
      ]
      where
        handleKey :: Int -> _
        handleKey 13 = dispatch $ ItemAction 0 $ ListQuery (Query.decode qRules s.queries)
        -- BS  8
        -- Ret 13
        -- C-d 68
        -- C-h 72
        -- C-m 77
        -- chrome系のエンジンだと日本語入力時のEnterでkeyupが発火しない問題
        --- electronもこれに準拠する模様
        handleKey _ = pure unit

main :: String -> Eff _ Unit
main bd = do
  os <- OS.platform
  let wd = ch os
  app <- toMaybe <$> HU.getElementById' (ElementId "item_browser")
  f <- lookupCMH' wd
  let cfs = Path.normalize (wd <> "/__config.json")
  chap <- approach (Path.normalize (wd <> f)) cfs
  TU.defaultMain specBrowser (testCMHFState chap) unit app
  where
    ch Darwin = Path.normalize (bd <> "/../../../../")
    ch _ = Path.normalize (bd <> "/../../")




catchBrowserQuery :: forall props
                     . T.Spec _ CMHFState props BrowserAction
                     -> T.Spec _ CMHFState props BrowserAction
catchBrowserQuery = over T._performAction \pa a p s ->
  case a of
    ItemAction _ (SetQuery qs) -> do
      TU.stateUpdate_ _queries qs
      TU.stateUpdate_ (_UIState <<< _queryString) (String.joinWith " " $ Query.decode qRules qs)
    ItemAction _ (ListQuery qs) -> do
      lift <<< liftEff <<< log <<< show $ qs
      js <- lift $ runStateT (listQueryP $ normalizeQS qs) s.outer
      TU.stateUpdate_ _OuterState $ snd js
      svit $ jsonToListInfoItem $ fst js
    ItemAction _ (ItemQuery ix) -> do 
      raw <- lift $ runStateT (rawQueryP ix) s.outer
      setp $ fst raw
      info <- lift $ runStateT (infoQueryP ix) $ snd raw
      setr $ fst info
      TU.stateUpdate_ _OuterState $ snd info
    _ -> pa a p s
  where
    svit (Right ls) = void $ T.cotransform (\state -> set (_HelperResult <<< _results) ls state)
    svit (Left e) = lift <<< liftEff <<< log <<< show $ e
    normalizeQS :: Array String -> Array String
    normalizeQS arr =
      case Array.uncons arr of -- もにょい
        Just {head: x, tail: _} | x == "lookup" || x == "find" -> arr
        _ -> Array.cons "lookup" $ map ("name=?" <> _) arr
    _raw' = _HelperResult <<< _raw
    _focus' = _HelperResult <<< _focus
    setp r@(Just _) = void $ T.cotransform (\state -> set _raw' r state)
    setp Nothing = pure unit
    setr j = void $ T.cotransform (\state -> set _focus' j state)
      
    
specRawJson :: forall eff props action . T.Spec eff HelperResult props action
specRawJson = T.simpleSpec T.defaultPerformAction render
  where
    render :: T.Render HelperResult props action
    render _ _ s _ =
      [ R.div
        [ P.className "raw-json" ]
        [ R.pre'
          [ R.code'
            [ R.text $ vif s.raw ]
          ]
        ]
      ]
    vif Nothing = "(none)"
    vif (Just rawJson) = rawJson



removal :: (InfoItemAction -> T.EventHandler) -> String -> Query -> ReactElement -> ReactElement
removal dispatch cls q body =
  R.span
    [ P.className cls ]
    [ R.span
      [ P.className "remove"
      , P.onClick \_ -> dispatch $ RemoveQuery q ]
      []
    , body ]

addrav :: (InfoItemAction -> T.EventHandler) -> String -> Query -> ReactElement -> ReactElement
addrav dispatch cls q body =
  R.span
    [ P.className cls ]
    [ R.span
      [ P.className "add"
      , P.onClick \_ -> dispatch $ AddQuery [q] ]
      []
    , body ]


specArrayQuery :: forall eff prop
                  . Array Query
                  -> T.Spec eff QueryHelperState prop InfoItemAction
specArrayQuery qs = T.simpleSpec T.defaultPerformAction render
  where
    render :: T.Render QueryHelperState prop InfoItemAction
    render d p s c =
      Array.fold (map (\r -> r d p s c) $ Query.display qdRules qs)
        
specQueryHelper :: forall eff prop . T.Spec eff QueryHelperState prop InfoItemAction
specQueryHelper = T.simpleSpec T.defaultPerformAction render
  where
    render :: T.Render QueryHelperState prop InfoItemAction -- or UIAction
    render dispatch p s c =
      [ R.div
        [ P.className "query-helper" ]
        (
          [ R.div
            [ P.className "qh-mode"]
            ((queryBuilder qdMode) dispatch p s c)
          ]
          <>
          (queryBuilder qdSort) dispatch p s c
          <>
          (queryBuilder qdNumOfItems) dispatch p s c
          <>
          (queryBuilder qdFilter) dispatch p s c
        )
      ]


qMode :: QueryRule Query
qMode = QueryRule
          (\l ->
            case l of
              Cons "find" xs -> Tuple xs (Just $ Mode Find)
              Cons "lookup" xs -> Tuple xs (Just $ Mode Lookup)
              _ -> Tuple l Nothing
          )
          ( case _ of
              Mode Find -> ["find"]
              Mode Lookup -> ["lookup"]
              _ -> []
          )
          (\q qs ->
            case q of
              Mode _ -> Just (
                [q] <> Array.filter (not (
                   case _ of
                     Mode _ -> true
                     _ -> false
                   )) qs
                )
              _ -> Nothing
          )
qdMode :: forall state props . QueryDisplayRule Query state props InfoItemAction
qdMode = QueryDisplayRule
          (\q ->
            case q of 
              Mode Find ->
                Just (\d _ _ _ -> [ removal d "mode find" q (R.text "原語検索") ])
              Mode Lookup ->
                Just (\d _ _ _ -> [ removal d "mode lookup" q (R.text "訳語検索") ])
              _ -> Nothing
          )
          (\d _ _ _ ->
            [ addrav d "mode find" (Mode Find) $ R.text "原語検索"
            , addrav d "mode lookup" (Mode Lookup) $ R.text "訳語検索"
            ]
          )

qSort :: QueryRule Query
qSort = QueryRule
          (\l ->
            case l of
              Cons "sort" (Cons "by" (Cons x xs)) ->
                Tuple xs (Just $ Sort $ Asc x)
              Cons "sort" (Cons "asc" (Cons "by" (Cons x xs))) ->
                Tuple xs (Just $ Sort $ Asc x)
              Cons "sort" (Cons "desc" (Cons "by" (Cons x xs))) ->
                Tuple xs (Just $ Sort $ Desc x)
              _ -> Tuple l Nothing
          )
          ( case _ of
              Sort (Asc  x) -> ["sort", "by", x]
              Sort (Desc x) -> ["sort", "desc", "by", x]
              _ -> []
          )
          (\q qs ->
            case q of
              Sort s -> Just (
                [q] <> Array.filter (not (
                  case _ of
                    Sort r ->
                      querySortBy r == querySortBy s
                    _ -> false
                  )) qs
                )
              _ -> Nothing
          )
qdSort :: forall props . QueryDisplayRule Query QueryHelperState props InfoItemAction
qdSort = QueryDisplayRule
           (\q ->
             case q of
               Sort (Asc  x) ->
                 Just (\d _ _ _ -> [ removal d "sort asc" q $ R.text ("昇順ソート: " <> x) ])
               Sort (Desc x) ->
                 Just (\d _ _ _ -> [ removal d "sort desc" q $ R.text ("降順ソート: " <> x) ])
               _ -> Nothing
           )
           (\d _ s _ ->
             [ addrav d (sortQueryClass s.sortDesc) ((sortQueryAction s.sortDesc) s.sortTarget) $
               R.span'
               [ R.text (sortQueryText s.sortDesc)
               , R.input
                 [ P._type "text" ]
                 [] -- inconmplete
               ]
             ]
           )
  where
    sortQueryClass false = "sort asc"
    sortQueryClass true = "sort desc"
    sortQueryText false = "昇順ソート: "
    sortQueryText true  = "降順ソート: "
    sortQueryAction false = Sort <<< Asc
    sortQueryAction true = Sort <<< Desc
qNumOfItems :: QueryRule Query
qNumOfItems = QueryRule
                (\l ->
                  case l of
                    Cons "up" (Cons "to" (Cons x xs)) ->
                      case Int.fromString x of
                        Just n ->
                          Tuple xs $ Just $ NumOfItems $ UpTo n
                        Nothing -> Tuple l Nothing
                    _ -> Tuple l Nothing
                )
                ( case _ of
                    NumOfItems (UpTo n) -> ["up", "to", intToString n]
                    _ -> []
                )
                (\q qs ->
                  case q of
                    NumOfItems _ -> Just (
                      [q] <> Array.filter (not (
                        case _ of
                          Mode _ -> true
                          _ -> false
                        )) qs
                      )
                    _ -> Nothing
                )
qdNumOfItems :: forall props . QueryDisplayRule Query QueryHelperState props InfoItemAction
qdNumOfItems = QueryDisplayRule
                 (\q ->
                   case q of
                     NumOfItems (UpTo n) ->
                       Just (\d _ _ _ ->
                              [ removal d "up-to" q $ R.text ("最大表示数: " <> intToString n) ]
                            )
                     _ -> Nothing
                 )
                 (\d _ s _ ->
                   [ addrav d "up-to" (NumOfItems (UpTo s.upto)) $
                     R.span'
                     [ R.text "最大表示数: "
                     , R.input
                       [ P._type "number" ]
                       []
                     ]
                   ]
                 )
qFilter :: QueryRule Query
qFilter = QueryRule
          (\l ->
            case l of
              Cons "mod" (Cons x xs) ->
                Tuple xs (Just $ Filter $ ModIdent x)
              _ -> Tuple l Nothing
          )
          ( case _ of
              Filter (ModIdent x) -> ["mod", x]
              _ -> []
          )
          (\q qs ->
            case q of
              Filter _ -> Just ( [q] <> Array.filter (_ /= q) qs )
              _ -> Nothing
          )
qdFilter :: forall props . QueryDisplayRule Query QueryHelperState props InfoItemAction
qdFilter = QueryDisplayRule
           (\q ->
             case q of
               Filter (ModIdent x) ->
                 Just (\d _ _ _ ->
                        [ removal d "filter mod" q $ R.text ("mod: " <> x) ]
                      )
               _ -> Nothing
           )
           (\d _ s _ ->
             [ addrav d "filter mod" (Filter (ModIdent s.filterMod)) $
               R.span'
               [ R.text "mod: "
               , R.input
                 [ P._type "text" ]
                 []
               ]
             ]
           )
qUnknown :: QueryRule Query
qUnknown = QueryRule
           (\l ->
             case l of
               Cons x xs -> Tuple xs (Just $ Unknown x)
               _ -> Tuple l Nothing
           )
           ( case _ of
               Unknown x -> [x]
               _ -> []
           )
           (\q qs ->
             case q of
               Unknown x -> Just ([q] <> qs)
               _ -> Nothing
           )
qdUnknown :: forall state props . QueryDisplayRule Query state props InfoItemAction
qdUnknown = QueryDisplayRule
            (\q ->
              case q of
                Unknown x ->
                  Just (\d _ _ _ ->
                         [ removal d "unknown" q $ R.text x ]
                       )
                _ -> Nothing
            )
            (\_ _ _ _ -> []) -- no unknown builder
qRules = List.fromFoldable [qMode, qNumOfItems, qSort, qFilter, qUnknown]
qdRules = List.fromFoldable [qdMode, qdNumOfItems, qdSort, qdFilter, qdUnknown]

{-
emrem :: Array String -> Array ReactElement
emrem qs = mode $ List.fromFoldable qs
  where
    expr (Cons "no" xs) =
      [ R.span [ P.className "no" ] [] ] <> filt xs -- .no:next {} で
    filt (Cons x xs) =
      [ R.span [ P.className "filter" ] [ R.text x ] ] <> expr xs
    undefined = [ R.span [ P.className "undefined" ] [ R.text "x"] ]
-}         


specNyoki :: forall eff props state
             . Lens' state Boolean
             -> T.Spec eff state props (UIAction state)
specNyoki _bool = T.simpleSpec paNyoki render
  where
    render :: T.Render state props (UIAction state)
    render dispatch _ s _ =
      [ R.div
        [ P.className ("nyoki " <> toggled (s ^. _bool) "nyoki-active" "nyoki-inactive") ]
        [ R.text "にょきっ" ]
      ]

paNyoki :: forall eff props state . T.PerformAction eff state props (UIAction state)
paNyoki (Nyoki _b b) _ _ = TU.stateUpdate_ _b b
paNyoki _ _ _ = pure unit


-- UI Component --
{-
mkSpecResizable :: forall eff state props action
                   . String -- | container class
                   -> String -- | paddle class
                   -> (Int -> {| style})
                   -> (P.Event -> Int) 
                   -> Lens' state Int
                   -> Prism' action (UIAction state)
                   -> T.Spec eff state props action
                   -> T.Spec eff state props action
mkSpecResizable containerClass paddleClass f _amount _UIA =
-}
  
-- 型安全でない
mkSpecResizableW :: forall eff state props action
                    . Lens' state Int
                    -> Prism' action (UIAction state)
                    -> T.Spec eff state props action
                    -> T.Spec eff state props action
mkSpecResizableW _width _pUIA = catchPaddleAction _pUIA <<< touchRender
  where
    touchRender :: T.Spec eff state props action
                   -> T.Spec eff state props action
    touchRender = over T._render \render dispatch p layout c ->
      [ R.div
        [ P.style {width: (intToString (layout ^. _width) <> "px")}
        , P.className "expand expand-w"
        ]
        (
          [ R.span
            [ P.className "paddle paddle-x"
            , P.draggable true
            , P.onDragEnd \e ->
            (dispatch <<< review _pUIA <<< PartialPaddlePos _width) (unsafeCoerce e).clientX
            , P.onDrag \e ->
            (dispatch <<< review _pUIA <<< PartialPaddlePos _width) (unsafeCoerce e).clientX
            ]
            []
          ]
          <> render dispatch p layout c
        )
      ]
mkSpecResizableH :: forall eff state props action
                    . Lens' state Int
                    -> Prism' action (UIAction state)
                    -> T.Spec eff state props action
                    -> T.Spec eff state props action
mkSpecResizableH _height _pUIA = catchPaddleAction _pUIA <<< touchRender
  where
    touchRender :: T.Spec eff state props action
                   -> T.Spec eff state props action
    touchRender = over T._render \render dispatch p layout c ->
      [ R.div
        [ P.style {height: (intToString (layout ^. _height) <> "px")}
        , P.className "expand expand-h"
        ]
        ( render dispatch p layout c <>
          [ R.div
            [ P.className "paddle paddle-y"
            , P.draggable true
            , P.onDragEnd \e ->
            (dispatch <<< review _pUIA <<< PartialPaddlePos _height) (unsafeCoerce e).clientY
            , P.onDrag \e ->
            (dispatch <<< review _pUIA <<< PartialPaddlePos _height) (unsafeCoerce e).clientY
            ]
            []
          ]
        )
      ]
catchPaddleAction :: forall eff state props action
                     . APrism' action (UIAction state)
                     -> T.Spec eff state props action
                     -> T.Spec eff state props action
catchPaddleAction _pUIA = over T._performAction \pa a p s ->
  case matching _pUIA a of
    Right (PartialPaddlePos _amount x) -> void (T.cotransform (\state -> set _amount x state))
    _ -> pa a p s






-- 内部状態 と 外部状態 とで分けたみある
type TranslationHelperState =
  { processing :: Boolean
  , target :: Maybe String
  , hover :: Boolean }
initialTranslationHelperState :: TranslationHelperState
initialTranslationHelperState = { processing: false, target: Nothing, hover: false }

data TranslationHelperAction
  = Translate (Either Error (List File))
  | Hover Boolean


specDropArea :: forall  props
                . T.Spec _ TranslationHelperState props TranslationHelperAction
specDropArea = T.simpleSpec performAction render
  where
    render :: T.Render TranslationHelperState props TranslationHelperAction
    render dispatch _ s _ =
      [ R.div
        [ P.className $ cls s.hover
        , P.onDragEnter \e -> do
          R.stopPropagation e
          R.preventDefault e
          (dispatch <<< Hover) true
        , P.onDragLeave \e -> do
          R.stopPropagation e
          R.preventDefault e
          (dispatch <<< Hover) false
        , P.onDrop \e -> do
          R.stopPropagation e
          R.preventDefault e
          (dispatch <<< Translate) $ dragEventToList e
        ]
        [ R.text $ lext s.target ]
      ]
    lext :: Maybe String -> String
    lext Nothing = "ここに翻訳したいModをドロップ"
    lext (Just s)  = "変換中...[" <> s <> "]"
    cls :: Boolean -> String
    cls false = "focus droparea"
    cls true  = "dragover focus droparea"
    performAction :: T.PerformAction _ TranslationHelperState props TranslationHelperAction
    performAction (Translate (Right fs)) _ _ = void $ do
      let t = (toMaybe <<< path) =<< head fs
      T.cotransform (_ { target = t, hover = false})
      lift $ mvp t
      T.cotransform (_ { target = Nothing})
    performAction (Translate (Left e)) _ _ = void $ do
      lift <<< liftEff <<< log <<< show $ e
    performAction (Hover b) _ _ = void $ T.cotransform (_ {hover = b})
    mvp :: Maybe String -> Aff _ Unit
    mvp (Just s) = void $ execTranslateAff s
    mvp Nothing  = liftEff $ log "no drop"
    -- Either Error String がいいな

dropIv = do
  app <- toMaybe <$> HU.getElementById' (ElementId "droparea")
  TU.defaultMain specDropArea initialTranslationHelperState unit app



