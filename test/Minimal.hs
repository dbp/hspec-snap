{-# LANGUAGE OverloadedStrings #-}

module Minimal where

import           Test.Hspec
import           Test.Hspec.Snap

main = hspec $ snap (return (SnapTest 1)) $ do
  it "Should do something" $ getRequest "invalid" >>= should200
  it "Should succeed again" $ getRequest "valid" >>= should200
