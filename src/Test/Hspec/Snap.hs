{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Hspec.Snap where

import           Control.Applicative     ((<$>))
import           Control.Concurrent.MVar
import           Control.Monad.State     (StateT (..), get, runStateT)
import           Control.Monad.Trans     (liftIO)
import           Data.Text               (Text)
import           Test.Hspec
import           Test.Hspec.Core

data TestResponse = Html | NotFound

data SnapTest = SnapTest Int deriving Show

type SnapState = StateT SnapTest IO

instance Example (SnapState Result) where
  type Arg (SnapState Result) = SnapTest
  evaluateExample s _ cb _ = do mv <- newEmptyMVar
                                cb $ \st -> do (r,_) <- runStateT s st
                                               putMVar mv r
                                takeMVar mv

afterAll :: IO () -> SpecWith a -> SpecWith a
afterAll action = go
  where go spec = do forest <- runIO $ runSpecM spec
                     res <- runIO $ mapM countFlatten forest
                     let specs = map snd res
                     let count = foldr (+) 0 (map fst res)
                     mvar <- runIO $ newMVar count
                     runIO $ print ("found this many specs: ", count)
                     after (\_ -> cleanup mvar) (fromSpecList specs)
        countFlatten :: SpecTree a -> IO (Int, SpecTree a)
        countFlatten (SpecGroup s t) =
          do (count, t') <- joinCount <$> mapM countFlatten t
             return (count, SpecGroup s t')
        countFlatten (BuildSpecs a) = do s <- a
                                         (count, s') <- joinCount <$> mapM countFlatten s
                                         return (count, BuildSpecs (return s'))
        countFlatten (SpecItem s i) = return (1, SpecItem s i)
        joinCount :: [(Int, b)] -> (Int, [b])
        joinCount = foldr (\(a,b) (c,d) -> (a + c, b:d)) (0, [])
        cleanup mv = modifyMVar mv $ \v -> do print ("count is ", v)
                                              if v == 1
                                                 then action >>= return . (v,)
                                                 else return (v - 1, ())

snap :: IO SnapTest -> SpecWith SnapTest -> Spec
snap app spec = do
  st <- runIO app
  afterAll (cleanup st) $ before (return st) spec
  where cleanup (SnapTest i) = print ("cleaning up ", i)

getRequest :: Text -> SnapState TestResponse
getRequest "valid" = do s <- get
                        liftIO $ print ("running get with state ", s)
                        return Html
getRequest _ = return NotFound

should200 :: TestResponse -> SnapState Result
should200 Html = return Success
should200 NotFound = return (Fail "Not found")
