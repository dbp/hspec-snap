{-# LANGUAGE OverloadedStrings         #-}

{-|
Module      : Utils
Description : Helpers for testing

Currently just copypasta from snap-extras to avoid dependency conflicts
-}
module Utils where

import           Data.Aeson      (encode, ToJSON)

import           Snap.Core       (modifyResponse, setHeader, writeLBS
                                 ,MonadSnap)


-------------------------------------------------------------------------------
-- | Set MIME to 'application/json' and write given object into
-- 'Response' body.
writeJSON :: (MonadSnap m, ToJSON a) => a -> m ()
writeJSON a = do
  jsonResponse
  writeLBS . encode $ a


-------------------------------------------------------------------------------
-- | Mark response as 'application/json'
jsonResponse :: MonadSnap m => m ()
jsonResponse = modifyResponse $ setHeader "Content-Type" "application/json"
