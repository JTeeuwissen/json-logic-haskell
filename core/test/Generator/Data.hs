module Generator.Data where

import Generator.Generic
import Hedgehog
import Hedgehog.Gen
import Hedgehog.Range as Range
import JsonLogic.Json

genRandomJson :: Gen Json
genRandomJson = undefined

genSizedRandomJson :: Size -> Gen Json
genSizedRandomJson (Size size)
  | size <= 0 =
      choice
        [ return JsonNull,
          fst <$> genGenericJsonBool,
          fst <$> genGenericJsonNumber,
          fst <$> genGenericJsonString
        ]
  | otherwise = undefined