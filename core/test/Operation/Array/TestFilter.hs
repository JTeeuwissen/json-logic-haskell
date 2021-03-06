{-# LANGUAGE OverloadedLists #-}

module Operation.Array.TestFilter where

import Generator.Logic
import Hedgehog as H (forAllWith, property, (===))
import qualified Hedgehog.Gen as Gen
import JsonLogic.Json
import JsonLogic.Pure.Evaluator
import Test.Tasty
import Test.Tasty.HUnit as U
import Utils

filterUnitTests :: TestTree
filterUnitTests =
  testGroup
    "Filter unit tests"
    [ testCase "Boolean filter values" $
        U.assertEqual
          "Return True elements"
          (Right $ JsonArray [JsonBool True, JsonBool True])
          ( apply
              []
              ( JsonObject
                  [ ( "filter",
                      JsonArray
                        [ JsonArray [JsonBool True, JsonBool False, JsonBool True],
                          JsonObject [("var", JsonString "")]
                        ]
                    )
                  ]
              )
              JsonNull
          ),
      testCase "Smaller than filter values" $
        U.assertEqual
          "Only the first item is < 2"
          (Right $ JsonArray [JsonNumber 1])
          ( apply
              []
              ( JsonObject
                  [ ( "filter",
                      JsonArray
                        [ JsonArray [JsonNumber 1, JsonNumber 3],
                          JsonObject [("<", JsonArray [JsonObject [("var", JsonString "")], JsonNumber 2])]
                        ]
                    )
                  ]
              )
              JsonNull
          ),
      testCase "applyuate array values" $
        U.assertEqual
          "The array value gets applyuated to True and returned."
          (Right $ JsonArray [JsonBool True])
          ( apply
              []
              ( JsonObject
                  [ ( "filter",
                      JsonArray
                        [ JsonArray
                            [ JsonBool False,
                              JsonObject [("<", JsonArray [JsonNumber 1, JsonNumber 2])]
                            ],
                          JsonObject [("var", JsonString "")]
                        ]
                    )
                  ]
              )
              JsonNull
          ),
      testCase "Empty array" $
        U.assertEqual
          "No errors occur when handed an empty array"
          (Right $ JsonArray [])
          ( apply
              []
              ( JsonObject
                  [ ( "filter",
                      JsonArray
                        [ JsonArray [],
                          JsonObject [("var", JsonString "")]
                        ]
                    )
                  ]
              )
              JsonNull
          ),
      testCase "Two consecutive filters" $
        U.assertEqual
          "Two consecutive filter more."
          (Right $ JsonArray [JsonNumber 2])
          ( apply
              []
              ( JsonObject
                  [ ( "filter",
                      JsonArray
                        [ JsonObject
                            [ ( "filter",
                                JsonArray
                                  [ JsonArray [JsonNumber 1, JsonNumber 2, JsonNumber 3],
                                    JsonObject [("<=", JsonArray [JsonObject [("var", JsonString "")], JsonNumber 2])]
                                  ]
                              )
                            ],
                          JsonObject [(">=", JsonArray [JsonObject [("var", JsonString "")], JsonNumber 2])]
                        ]
                    )
                  ]
              )
              JsonNull
          )
    ]

filterGeneratorTests :: TestTree
filterGeneratorTests =
  testGroup
    "filter generator tests"
    [ hTestProperty "Using double array" $
        property $ do
          -- Generate random data
          ((opJson, op), (arrayJson, array)) <- forAllWith (show . \((a, _), c) -> (a, c)) $ Gen.sized sizedGenNumericArrayComparisonJson
          -- Create the rule
          let rule = JsonObject [("filter", JsonArray [arrayJson, opJson])]
          Right (JsonArray (map JsonNumber (filter op array))) === apply [] rule JsonNull
    ]
