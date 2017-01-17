
module Main.Data ( hrSetTestData
                 , testCMHFState
                 , testJson
                 , module Main.Data.States
                 , module Main.Data.Actions)where

import Data.List (List(Nil), (:))

import Main.Data.States
import Main.Data.Actions

hrSetTestData :: HelperResult -> HelperResult
hrSetTestData hr = hr { results = ls }
  where
    ls = (mockInfoItem "abc") : (mockInfoItem "123") : Nil

testCMHFState = initialCMHFState initialBrowserLayout (hrSetTestData initialHelperResult)



testJson :: String
testJson = """
{ "type": "DUMMY_ITEM"
, "name": "abc"
}
"""
