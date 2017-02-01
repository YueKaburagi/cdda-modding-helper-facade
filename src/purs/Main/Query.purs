
module Main.Query where

import Prelude
import Data.Array as Array
import Data.List (List, List(Cons, Nil))
import Data.List as List
import Data.Tuple
import Data.Maybe

import React (ReactElement)
import Thermite (Render)

type EncodeRule query = List String -> Tuple (List String) (Maybe query)
type DecodeRule query = query -> Array String
type AddRule query = query -> Array query -> Maybe (Array query)
data QueryRule query =
  QueryRule
    (EncodeRule query)
    (DecodeRule query)
    (AddRule query)

type DisplayRule query state props action = query -> Maybe (Render state props action)
type BuilderDisplayRule state props action = Render state props action -- | alias
data QueryDisplayRule query state props action =
  QueryDisplayRule
    (DisplayRule query state props action)
    (BuilderDisplayRule state props action)


queryEncoder :: forall query . QueryRule query -> EncodeRule query
queryEncoder (QueryRule e _ _) = e
queryDecoder :: forall query . QueryRule query -> DecodeRule query
queryDecoder (QueryRule _ d _) = d
queryAdder :: forall query . QueryRule query -> AddRule query
queryAdder (QueryRule _ _ a) = a

queryDisplayer :: forall q s p a . QueryDisplayRule q s p a -> DisplayRule q s p a
queryDisplayer (QueryDisplayRule d _) = d
queryBuilder :: forall q s p a . QueryDisplayRule q s p a -> BuilderDisplayRule s p a
queryBuilder (QueryDisplayRule _ b) = b

encode :: forall query . List (QueryRule query) -> Array String -> Array query
encode rules qs = ektek rules $ List.fromFoldable qs
  where
    ektek _ Nil = []
    ektek (Cons x xs) ls = pass xs $ (queryEncoder x) ls
    ektek Nil _ = [] -- no rule found
    pass _ (Tuple ls (Just x)) = [x] <> ektek rules ls
    pass xrs (Tuple ls Nothing) = ektek xrs ls

decode :: forall query . List (QueryRule query) -> Array query -> Array String
decode rules qs = Array.fold (srm rules <$> qs)
  where
    srm (Cons x xs) q = pass xs q $ (queryDecoder x) q
    srm Nil _ = [] -- no rule found
    pass xrs q [] = srm xrs q
    pass _ _ x = x

add :: forall query . List (QueryRule query) -> query -> Array query -> Array query
add rules q qs = ip (queryAdder <$> rules)
  where
    ip (Cons x xs) = pass xs $ x q qs
    ip Nil = qs -- no rule found
    pass xrs Nothing = ip xrs
    pass _ (Just x) = x
addF :: forall query . List (QueryRule query) -> Array query -> Array query -> Array query
addF rules srcs qs = mvf (List.fromFoldable srcs) qs
  where
    mvf Nil rr = rr
    mvf (Cons x xs) rr = mvf xs $ add rules x rr

display :: forall query state props action
           . List (QueryDisplayRule query state props action)
           -> Array query
           -> Array (Render state props action)
display rules qs = Array.catMaybes (uk rules <$> qs)
  where
    uk (Cons x xs) q = pass xs q $ (queryDisplayer x) q
    uk Nil _ = Nothing -- no rule found
    pass xrs q Nothing = uk xrs q
    pass _ _ x = x

