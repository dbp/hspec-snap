{-# LANGUAGE OverloadedStrings         #-}

{-|
Module      : Utils
Description : Helpers for testing

-}
module Utils where

import           Data.Aeson              (decode, encode, FromJSON, ToJSON)
import           Control.Applicative     ((<$>))

import           Snap.Core               (modifyResponse, readRequestBody
                                         ,setHeader, writeLBS ,MonadSnap)

-- | Attempts to parse JSON from a request body of max 1MiB
parseJsonBody :: (MonadSnap m, FromJSON fj) => m (Maybe fj)
parseJsonBody = decode <$> readRequestBody 1048576

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
