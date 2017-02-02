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
import Data.List (List(Cons, Nil), head)
import Data.List as List
import Data.Array ((!!))
import Data.Array as Array
import Data.Either (Either (..))
import Data.Foldable (fold)
import Data.String (split, joinWith) as String
import Data.String.Regex (regex, Regex)
import Data.String.Regex (split, match) as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.Int as Int

import DOM.Node.Types (ElementId (..))
import DOM (DOM)
import DOM.File.Types (File)

import Node.ChildProcess (CHILD_PROCESS, ChildProcess)
import Node.Buffer (BUFFER)

import Main.Data
import Main.Data.Query
import Main.Query (QueryDisplayRule(..), queryBuilder)
import Main.Query as Query
import Main.Rule.Query as QueryRule
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
import Data.Argonaut.Core (toObject, toNumber, toString,
                           fromNumber, fromString) as Json
import Text.Format as Format

import Unsafe.Coerce (unsafeCoerce)

import Node.OS (Platform(Darwin), OS)
import Node.OS (platform) as OS
import Node.Path (normalize) as Path


renderInfoItem :: forall props . T.Render InfoItem props BrowserAction
renderInfoItem dispatch _ mi _ = 
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
specResultPane = T.focusState _results $ T.simpleSpec T.defaultPerformAction render
  where
    render d p s c =
      [ R.ul' $ (TU.list renderInfoItem) d p s c ]


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
    mkSpecResizableW (_UIState <<< _BrowserLayout <<< _resultPaneWidth) _UIAction 
      (T.focus _HelperResult _BrowserAction specResultPane)
    <>
    specContain "expand-w-tail" (
      mkSpecResizableH (_UIState <<< _BrowserLayout <<< _itemInfoHeight) _UIAction
        (T.focus (_HelperResult <<< _focus) _BrowserAction specMayItem)
      <>
      T.focusState _HelperResult specRawJson
    )
  )
  <>
  specSearchArea
  <>
  T.match _BrowserAction (T.simpleSpec paBrowserAction T.defaultRender)
  <>
  T.match _UIAction (T.simpleSpec paUIAction T.defaultRender)  

makeModNameInfo :: (BrowserAction -> T.EventHandler) -> JObject -> Array ReactElement
makeModNameInfo d j =
  case Json.toString =<< lookup "modname" j of
    Just n -> [ R.a
                ( [ P.className "info modname" ]
                  <>
                  makeModIdentQuery d j
                )
                [ R.text n ]
              ]
    Nothing -> []
makeModIdentQuery :: (BrowserAction -> T.EventHandler) -> JObject -> Array Props
makeModIdentQuery dispatch j =
  case Json.toString =<< lookup "modident" j of
    Just x -> [ P.onClick \_ -> dispatch $ AddQuery [Filter $ ModIdent x]
              , P.title ("このmod(" <> x <> ")で絞り込む") ]
    Nothing -> []
makeVolumeInfo :: Maybe JObject -> Array ReactElement
makeVolumeInfo j =
  case Json.toNumber =<< lookup "volume" =<< j of
    Just n -> mkv $ rescale 0 (n * 0.25 / stackSize j)
    Nothing -> []
  where
    mkv (Tuple i n) = [ R.span
                        [ P.className "volume" ]
                        [ R.text ("体積: " <> Format.format (formatter n) n <> " " <> unitof i)] ]
    unitof i = maybe "?l" id $ ["pl", "ml", "l", "kl", "Ml"] !! (i + 2)
makeWeightInfo :: Maybe JObject -> Array ReactElement
makeWeightInfo j =
  case Json.toNumber =<< lookup "weight" =<< j of
    Just n -> mkw $ rescale 0 n
    Nothing -> []
  where
    mkw (Tuple i n) = [ R.span
                        [ P.className "weight" ]
                        [ R.text ("質量: " <> Format.format (formatter n) n <> " " <> unitof i)] ]
    unitof i = maybe "?g" id $ ["mg", "g", "kg", "t", "kt"] !! (i + 1)
rescale :: Int -> Number -> Tuple Int Number
rescale i n | n < 1.0 = rescale (i - 1) (n * 1000.0)
            | n >= 1000.0 = rescale (i + 1) (n * 0.001)
            | otherwise = Tuple i n
formatter :: Number -> Format.Properties
formatter n | n < 10.0 = Format.precision 2
            | n < 100.0 = Format.precision 1
            | otherwise = Format.precision 0
stackSize :: Maybe JObject -> Number
stackSize (Just body) =
  case lookup "stack_size" body of
    Nothing ->
      case lookup "charges" body of
        Nothing -> 1.0
        Just n -> maybe 1.0 id $ Json.toNumber n
    Just n -> maybe 1.0 id $ Json.toNumber n
stackSize Nothing = 1.0
-- 自動で className つけていいような？
-- action は BrowserAction ？
makeItemInfo :: (BrowserAction -> T.EventHandler) -> JObject -> Array ReactElement
makeItemInfo dispatch j =
  makeModNameInfo dispatch j <>
--  mkt'  (Just j) "modname" (R.span [P.className "info-modname"]) "" "" <>
  spn'' b "name" <>
  makeVolumeInfo b <>
  makeWeightInfo b <>
  -- melee
  spn'  b "to_hit" "命中: " "" <>
  spn'  b "bashing" "打撃: " "" <>
  spn'  b "cutting" "切断: " "" <>
  -- general
  spn'  b "fun" "満喫: " "" <>
  -- food (med)
  spn'  b "quench" "水分: " "" <>
  spn'  b "nutrition" "満腹: " "" <>
  spn'  b "healthy" "健康: " "" <> -- spoil
  spn'  b "stim" "神経作用: " "" <> -- spoil
  spn'  b "addiction_potential" "依存性: " "" <> -- spoil
  -- gun
  spn'  b "range" "射程修整: " "" <>
  spn'  b "ranged_damage" "追加威力: " "" <>
  spn'  b "dispersion" "分散: " "" <>
  spn'  b "durability" "堅牢性: " "/10" <>
  spn'  b "burst" "" " 連バースト" <>
  spn'  b "clip_size" "装填数: " "" <>
  spn'  b "ups_charges" "消費電力: " "" <>
  spn'  b "reload" "装填時間: " "" <>
  spn'  b "loudness" "音: " "" <>
  -- ammo
  spn'  b "damage" "威力修整: " "" <>
  spn'  b "pirece" "貫通修整: " "" <>
  -- clothe (armor)
  spn'  b "coverage" "被覆率: " " %" <>
  spn'  b "encumbrance" "動作制限: " "" <>
  spn'  b "warmth" "暖かさ: " "" <>
  spn'  b "environmental_protection" "環境防護: " "" <>
  -- general
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
    

specMayItem :: forall eff props . T.Spec _ Json props BrowserAction
specMayItem = T.simpleSpec T.defaultPerformAction render
  where
    render :: T.Render Json props BrowserAction
    render dispatch _ json _ =
      [ R.div
        [ P.className "json-info" ]
        (foldJsonObject [] (makeItemInfo dispatch) json)
      ]



toggled :: forall a . Boolean -> a -> a -> a
toggled true  t _ = t
toggled false _ f = f


specSearchArea :: forall props . T.Spec _ CMHFState props CMHFAction
specSearchArea =
  specSearchAreaF 
    (_UIState <<< _QueryHelperState <<< _nyokking)
    (specIndicator
     <>
     T.focusState _UIState specSearchBar
     <>
     T.focusState (_UIState <<< _QueryHelperState) specNyoki)
    
specSearchAreaF :: forall eff props
                  . Setter' CMHFState Boolean
                  -> T.Spec _ CMHFState props CMHFAction
                  -> T.Spec _ CMHFState props CMHFAction
specSearchAreaF _b = over T._render \render dispatch p s c ->
  [ R.div
    [ P.className "search-area"
    , P.onMouseLeave \_ -> dispatch $ UIAct $ UpdateBool _b false
    ]
    (render dispatch p s c)
  ]

specIndicator :: forall eff props . T.Spec eff CMHFState props CMHFAction
specIndicator = T.simpleSpec T.defaultPerformAction render
  where
    render :: T.Render CMHFState props CMHFAction
    render dispatch p s c =
      [ R.div
        [ P.className "query-info" ]
        [ R.button
          [ P.className "search-button"
          , P.onClick \_ -> dispatch $ BrAct $ SendQueryString ]
          [ R.img [ P.src "./img/search_left.svg"
                  , P.className "search"
                  , P.alt "=>"
                  , P.title "検索する"
                  ] [] ]
        , R.div
          [ P.className "query-indicator" ]
          (
            (renderArrayQuery s.queries) dispatch p (s ^. _UIState ^. _QueryHelperState) c
          )
        ]
      ]
specSearchBar :: forall eff props . T.Spec eff UIState props CMHFAction
specSearchBar = T.simpleSpec T.defaultPerformAction render
  where
    render :: T.Render UIState props CMHFAction
    render dispatch p s c =
      [ R.div
        [ P.className (
             "search-bar "
             <>
             toggled (s ^. _QueryHelperState ^. _nyokking) "search-bar-hide" "search-bar-show") ]
        [ R.input
          [ P._type "text"
          , P.className "search"
          , P.value (s ^. _queryString)
          , P.onMouseOver \_ -> dispatch $ UIAct $ UpdateBool (_UIState <<< _QueryHelperState <<< _nyokking) true
          , P.onKeyUp \e -> handleKey (unsafeCoerce e).keyCode (unsafeCoerce e).target.value
          , P.onChange \e -> dispatch $ BrAct $ ChangeQuery (unsafeCoerce e).target.value
          ]
          []
        ]
      ]
      where
        handleKey :: Int -> _ -> _
        handleKey 13 _ = dispatch $ BrAct $ SendQueryString
        -- BS  8
        -- Ret 13
        -- C-d 68
        -- C-h 72
        -- C-m 77
        -- chrome系のエンジンだと日本語入力時のEnterでkeyupが発火しない問題
        --- electronもこれに準拠する模様
        handleKey _ _ = pure unit

specNyoki :: forall eff props . T.Spec eff QueryHelperState props CMHFAction
specNyoki = T.simpleSpec T.defaultPerformAction render
  where
    render dispatch p s c =
      [ R.div
        [ P.className ("nyoki "
                       <>
                       toggled (s ^. _nyokking) "nyoki-active" "nyoki-inactive"
                      )
        ]
        (renderQueryHelper dispatch p s c)
      ]
      

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




normalizeQueris :: Array Query -> Array Query
normalizeQueris qs =
  case Array.any anyMode qs of
    true -> qs
    false -> Query.add QueryRule.rules (Mode Find) qs
  where
    anyMode (Mode _) = true
    anyMode _ = false

paUIAction :: forall eff props state . T.PerformAction eff state props (UIAction state)
paUIAction (UpdateBool _b b) _ _ = TU.stateUpdate_ _b b
paUIAction (UpdateString _s s) _ _ = TU.stateUpdate_ _s s
paUIAction _ _ _ = pure unit

-- query control
paBrowserAction :: forall eff props . T.PerformAction _ CMHFState props BrowserAction
paBrowserAction (ChangeQuery qs) _ _ = do
  TU.stateUpdate_ (_UIState <<< _queryString) qs
  ss <- lift <<< liftEff <<< sep $ qs
  TU.stateUpdate_ _queries $ Query.encode QueryRule.rules ss
    where
      sep :: String -> Eff _ (Array String)
      sep s = do
        r <- esToEff $ regex "\\s+" RegexFlags.global
        pure $ Array.filter (_ /= "") $ Regex.split r s
paBrowserAction (SetQuery qs) p s = do
  TU.stateUpdate_ _queries qs
  TU.stateUpdate_ (_UIState <<< _queryString) (String.joinWith " " $ Query.decode QueryRule.rules qs)
paBrowserAction (AddQuery qs) p s = do
  TU.stateUpdate_ (_UIState <<< _QueryHelperState <<< _sortTarget) ""
  TU.stateUpdate_ (_UIState <<< _QueryHelperState <<< _key) ""
  TU.stateUpdate_ (_UIState <<< _QueryHelperState <<< _value) ""
  TU.stateUpdate_ (_UIState <<< _QueryHelperState <<< _filterMod) ""
  void $ T.cotransform (\state -> set _queries (normalizeQueris $
                           Query.addF QueryRule.rules qs (state ^. _queries)
                                               ) state)
  paBrowserAction FlushQuery p s
paBrowserAction (RemoveQuery q) p s = do
  void $ T.cotransform (\state -> set _queries (Array.filter (_ /= q) state.queries) state)
  paBrowserAction FlushQuery p s
paBrowserAction FlushQuery _ _ =
  void $ T.cotransform (\state ->
                         set
                         (_UIState <<< _queryString)
                         (String.joinWith " " $ Query.decode QueryRule.rules (state ^. _queries))
                         state)
paBrowserAction SendQueryString p s =
  paBrowserAction (SendQuery $ Query.decode QueryRule.rules (s ^. _queries)) p s
paBrowserAction (SendQuery qs) _ s = do
  lift <<< liftEff <<< log <<< show $ qs
  js <- lift $ runStateT (listQueryP $ normalizeQS qs) s.outer
  TU.stateUpdate_ _OuterState $ snd js
  svit $ jsonToListInfoItem $ fst js
  where
    svit (Right ls) =
      TU.stateUpdate_ (_HelperResult <<< _results) (List.reverse ls) -- reversed at jsonToListInfoItem
    svit (Left e) = lift <<< liftEff <<< log <<< show $ e
    normalizeQS :: Array String -> Array String
    normalizeQS arr =
      case Array.uncons arr of
        Just {head: x, tail: _} | x == "lookup" || x == "find" -> arr
        _ -> Array.cons "lookup" $ map ("name=?" <> _) arr
paBrowserAction (ItemQuery ix) _ s = do
  raw <- lift $ runStateT (rawQueryP ix) s.outer
  setp $ fst raw
  info <- lift $ runStateT (infoQueryP ix) $ snd raw
  setr $ fst info
  TU.stateUpdate_ _OuterState $ snd info
  where
    setp r@(Just _) = TU.stateUpdate_ (_HelperResult <<< _raw) r
    setp Nothing = pure unit
    setr j = TU.stateUpdate_ (_HelperResult <<< _focus) j
paBrowserAction _ _ _ = pure unit



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




renderArrayQuery :: forall prop
                  . Array Query
                  -> T.Render QueryHelperState prop CMHFAction
renderArrayQuery qs d p s c =
  Array.fold $ map (\r -> r d p s c) $ Query.display qdRules qs
        
renderQueryHelper :: forall prop . T.Render QueryHelperState prop CMHFAction
renderQueryHelper d p s c =
      [ R.div
        [ P.className "query-helper" ]
        (
          row ( [R.span' [R.text "検索モード選択:"]] <> (queryBuilder qdMode) d p s c)
          <>
          row ( [R.span' [R.text "絞り込み条件:"]] <> (queryBuilder qdFilter) d p s c)
          <>
          row ( [R.span' [R.text "表示上限:"]] <> (queryBuilder qdNumOfItems) d p s c)
          <>
          row ( [R.span' [R.text "ソート:"]] <> (queryBuilder qdSort) d p s c)
        )
      ]
  where
    row es = [ R.div [ P.className "row" ] es ]

removal :: (CMHFAction -> T.EventHandler) -> String -> Query -> ReactElement -> ReactElement
removal dispatch cls q body =
  R.span
    [ P.className cls ]
    [ R.button
      [ P.onClick \_ -> dispatch $ BrAct $ RemoveQuery q
      , P.tabIndex (-1)
      , P.className "remove" ]
      [ R.img
        [ P.className "remove"
        , P.src "./img/close_mini.svg"
        ]
        []
      ]
    , body ]

addrav :: (CMHFAction -> T.EventHandler) -> String -> Query -> ReactElement -> ReactElement
addrav dispatch cls q body =
  R.span
    [ P.className cls ]
    [ R.button
      [ P.onClick \_ -> dispatch $ BrAct $ AddQuery [q]
      , P.tabIndex (-1)
      , P.className "add" ]
      [ R.img
        [ P.className "add"
        , P.src "./img/plus_mini.svg"
        ]
        []
      ]
    , body ]


qdMode :: forall state props . QueryDisplayRule Query state props CMHFAction
qdMode = QueryDisplayRule
          (\q ->
            case q of 
              Mode Find ->
                Just $ \d _ _ _ -> [ removal d "mode find" q $ R.span' [ R.text "原語" ] ]
              Mode Lookup ->
                Just $ \d _ _ _ -> [ removal d "mode lookup" q $ R.span' [ R.text "訳語" ] ]
              _ -> Nothing
          )
          (\d _ _ _ ->
              [ addrav d "mode find" (Mode Find) $
                R.span
                [ P.title "書かれた条件でそのまま検索します" ]
                [ R.text "原語検索" ]
              , addrav d "mode lookup" (Mode Lookup) $ R.span
                [ P.title "原語以外(日本語など)で検索します" ]
                [ R.text "訳語検索" ]
              ]
          )

qdSort :: forall props . QueryDisplayRule Query QueryHelperState props CMHFAction
qdSort = QueryDisplayRule
           (\q ->
             case q of
               Sort (Asc  x) ->
                 Just $ \d _ _ _ ->
                 [ removal d "sort asc" q $ R.span' [
                      R.span' [ R.text ":" ]
                      , R.button [ P.onClick \_ -> d $ BrAct $ AddQuery [ Sort $ Desc x ]
                                 , P.tabIndex (-1)
                                 ] [ sortIcon false ]
                      , R.span' [ R.text (" " <> x) ]
                      ]
                 ]
               Sort (Desc x) ->
                 Just $ \d _ _ _ ->
                 [ removal d "sort desc" q $ R.span' [
                      R.span' [ R.text ":" ]
                      , R.button [ P.onClick \_ -> d $ BrAct $ AddQuery [ Sort $ Asc x ]
                                 , P.tabIndex (-1)
                                 ] [ sortIcon true ]
                      , R.span' [ R.text (" " <> x) ]
                      ]
                 ]
               _ -> Nothing
           )
           (\d _ s _ ->
             [ addrav d (sortQueryClass s.sortDesc) ((sortQueryAction s.sortDesc) s.sortTarget) $
               R.span'
               [ R.span'
                 [ R.span' [ R.text "ソート" ]
                 , R.button [ P.onClick \_ -> d $ UIAct $ UpdateBool
                                              (_UIState <<< _QueryHelperState <<< _sortDesc)
                                              (not s.sortDesc)
                            , P.tabIndex (-1)
                            ] [ sortIcon s.sortDesc ]
                 , R.span' [ R.text ":" ]
                 ]
               , R.input
                 [ P._type "text"
                 , P.onChange \e -> d $ UIAct $ UpdateString
                                    (_UIState <<< _QueryHelperState <<< _sortTarget)
                                    (unsafeCoerce e).target.value
                 , P.value s.sortTarget ]
                 [] -- inconmplete
               ]
             ]
           )
  where
    sortIcon false = R.img [ P.src "./img/object_alignment_round.svg"
                            , P.className "asc"
                            , P.alt "昇順"
                            , P.title "昇順ソート"
                            ] []
    sortIcon true = R.img [ P.src "./img/object_alignment_round.svg"
                            , P.className "desc"
                            , P.alt "降順"
                            , P.title "降順ソート"
                            ] []    
    sortQueryClass false = "sort asc"
    sortQueryClass true = "sort desc"
    sortQueryAction false = Sort <<< Asc
    sortQueryAction true = Sort <<< Desc
qdNumOfItems :: forall props . QueryDisplayRule Query QueryHelperState props CMHFAction
qdNumOfItems = QueryDisplayRule
                 (\q ->
                   case q of
                     NumOfItems (UpTo n) ->
                       Just $ \d _ _ _ ->
                         [ removal d "up-to" q $ R.span' [ R.text ("最大表示数: " <> intToString n) ]]
                     _ -> Nothing
                 )
                 (\d _ s _ ->
                   [ addrav d "up-to" (NumOfItems (UpTo s.upto)) $
                     R.span [ P.title "検索結果の上限を設定します" ]
                     [ R.span' [ R.text "最大表示数: " ]
                     , R.input
                       [ P._type "number"
                       , P.maxLength "2"
                       , P.max "99" -- evade buffer over error
                       , P.min "0"
                       , P.step "5"
                       , P.value (s ^. _upto)
                       , P.className "upto"
                       , P.onChange \e -> d $ UIAct $ UpdateString
                                          (_UIState <<< _QueryHelperState <<< _upto)
                                          (unsafeCoerce e).target.value
                       ]
                       []
                     ]
                   ]
                 )
qdFilter :: forall props . QueryDisplayRule Query QueryHelperState props CMHFAction
qdFilter = QueryDisplayRule
           (\q ->
             case q of
               Filter (No p) -> 
                 Just $ \d _ _ _ ->
                   [ removal d "no" q $ disp p ]
               Filter p ->
                 Just $ \d _ _ _ ->
                   [ removal d "filter" q $ disp p ]
               _ -> Nothing
           )
           (\d _ s _ ->
             [ addrav d "filter mod" (Filter (ModIdent s.filterMod)) $
               R.span [ P.title "どの mod かで絞り込む" ]
               [ R.span' [ R.text "mod: " ]
               , R.input
                 [ P._type "text"
                 , P.value s.filterMod
                 , P.onChange \e -> d $ UIAct $ UpdateString
                                    (_UIState <<< _QueryHelperState <<< _filterMod)
                                    (unsafeCoerce e).target.value
                 ]
                 []
               ]
             , addrav d "filter field" (mkq s.key s.value) $
               R.span
               [ P.title $ String.joinWith "\n"
                          [ "`key =` 特定のキーを持っている"
                          , "`= value` 特定の値を持っている"
                          , "`= ?value` その値をどこかに含んでいる"
                          , "`key = value` 特定のキーと値の組を持っている"
                          ]
               ]
               [ R.input
                 [ P._type "text"
                 , P.value s.key
                 , P.className "key"
                 , P.onChange \e -> d $ UIAct $ UpdateString
                                    (_UIState <<< _QueryHelperState <<< _key)
                                    (unsafeCoerce e).target.value
                 ]
                 []
               , R.text " = "
               , R.input
                 [ P._type "text"
                 , P.value s.value
                 , P.className "value"
                 , P.onChange \e -> d $ UIAct $ UpdateString
                                    (_UIState <<< _QueryHelperState <<< _value)
                                    (unsafeCoerce e).target.value
                 ]
                 []
               ]
             ]
           )
  where
    disp q =
      case q of
        ModIdent x ->
          R.span [ P.className "mod" ] [ R.text ("mod: " <> x) ]
        HasField k v ->
          R.span [ P.className "field" ] [ R.text (k <> "=" <> v) ]
        HasKey k ->
          R.span [ P.className "key" ] [ R.text (k <> "=") ]
        HasValue v ->
          R.span [ P.className "value" ] [ R.text ("=" <> v) ]
        _ ->
          R.span' [ R.text "???" ]
    mkq "" "" = Unknown ""
    mkq "" v = Filter (HasValue v)
    mkq k "" = Filter (HasKey k)
    mkq k v = Filter (HasField k v)
qdUnknown :: forall state props . QueryDisplayRule Query state props CMHFAction
qdUnknown = QueryDisplayRule
            (\q ->
              case q of
                Unknown x ->
                  Just $ \d _ _ _ ->
                    [ removal d "unknown" q $ R.span' [ R.text x ]]
                _ -> Nothing
            )
            (\_ _ _ _ -> []) -- no unknown builder

qdRules = List.fromFoldable [qdMode, qdNumOfItems, qdSort, qdFilter, qdUnknown]


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



