{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Dump.DumpSpec (spec) where

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

{- HLINT ignore "Redundant do"        -}

spec :: Spec
spec = describe "HaskellWorks.Data.Bits.DumpSpec" $ do
  it "Empty" $ requireTest $ do
    True === True
