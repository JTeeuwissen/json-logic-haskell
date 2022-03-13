module Utils where

import Hedgehog.Internal.Property
import Test.Tasty
import qualified Test.Tasty.Hedgehog as H

hTestProperty :: TestName -> Property -> TestTree
hTestProperty name = H.testPropertyNamed name (PropertyName name)
