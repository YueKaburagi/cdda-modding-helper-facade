
module Main.Rule.Query (rules) where

import Prelude
import Data.Array ((!!))
import Data.Array as Array
import Data.List as List
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Either (Either(Right))
import Data.Tuple (Tuple(..))
import Data.String.Regex (regex, Regex)
import Data.String.Regex (match) as Regex
import Data.String.Regex.Flags (noFlags) as RegexFlags
import Data.Int (fromString) as Int

import Util (intToString)
import Main.Data.Query
import Main.Query (QueryRule(..))

rules = List.fromFoldable [qMode, qNumOfItems, qSort, qFilter, qUnknown]


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
                Array.filter (not (
                  case _ of
                    Sort r ->
                      querySortBy r == querySortBy s
                    _ -> false
                  )) qs <> [q]
                ) 
              _ -> Nothing
          )
          
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
                      Array.filter (not (
                        case _ of
                          NumOfItems _ -> true
                          _ -> false
                        )) qs <> [q]
                      ) 
                    _ -> Nothing
                )
                
qFilter :: QueryRule Query
qFilter = QueryRule
          encoder
          decoder
          (\q qs ->
            case q of
              Filter _ -> Just ( Array.filter (_ /= q) qs <> [q] )
              _ -> Nothing
          )
  where
    decoder q =
      case q of
        Filter (No x) ->
          case decoder (Filter x) of
            [] -> []
            o -> ["no"] <> o
        Filter (ModIdent x) -> ["mod", x]
        Filter (HasField k v) -> [k <> "=" <> v]
        Filter (HasKey k) -> [k <> "="]
        Filter (HasValue v) -> ["=" <> v]
        _ -> []
    encoder l =
      case l of
        Cons "no" xs -> 
          case encoder xs of
            Tuple ys (Just (Filter x)) -> Tuple ys (Just $ Filter $ No x)
            _ -> Tuple l Nothing
        Cons "mod" (Cons x xs) ->
          Tuple xs (Just $ Filter $ ModIdent x)
        Cons x xs ->
          case patts of
            Right ps ->
              matty l xs ps x
            _ -> Tuple l Nothing
        _ -> Tuple l Nothing
    matty l _ Nil _ = Tuple l Nothing
    matty l xs (Cons (Tuple m b) fs) x =
      case Regex.match m x of
        Just arr -> b l xs (join (arr !! 1)) (join (arr !! 2))
        _ -> matty l xs fs x
    patts = do
      f <- field
      k <- key
      v <- value
      pure $  List.fromFoldable [Tuple f mField, Tuple k mKey, Tuple v mValue]
    field = regex """^([^\s]+?)=([^\s]+)$""" RegexFlags.noFlags
    key = regex """^([^\s]+)=$""" RegexFlags.noFlags
    value = regex """^=([^\s]+)$""" RegexFlags.noFlags
    mField _ xs (Just k) (Just v) = Tuple xs (Just $ Filter $ HasField k v)
    mField l _ _ _ = Tuple l Nothing
    mKey _ xs (Just k) _ = Tuple xs (Just $ Filter $ HasKey k)
    mKey l _ _ _ = Tuple l Nothing
    mValue _ xs (Just v) _ = Tuple xs (Just $ Filter $ HasValue v)
    mValue l _ _ _ = Tuple l Nothing

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
               Unknown x -> Just (qs <> [q])
               _ -> Nothing
           )
