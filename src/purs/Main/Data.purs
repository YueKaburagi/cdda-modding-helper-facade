
module Main.Data ( hrSetTestData
                 , testCMHFState
                 , testJson
                 , module Main.Data.States
                 , module Main.Data.Actions)where

import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(Nothing))

import Main.Data.States
import Main.Data.Actions

hrSetTestData :: HelperResult -> HelperResult
hrSetTestData hr = hr { results = his }
  where
    ls = (mockInfoItem "abc") : (mockInfoItem "123") : Nil
    his = (mkInfoItem Nothing "DDA" (CATQuery ["find", "mod", "dda"])) :
          (mkInfoItem Nothing "monsters" (CATQuery ["find", "type=MONSTER"])) : Nil

testCMHFState = initialCMHFState initialBrowserLayout (hrSetTestData initialHelperResult)



testJson :: String
testJson = """
{ "type": "DUMMY_ITEM"
, "name": "abc"
}
"""
