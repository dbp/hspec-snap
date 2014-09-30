{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}

module Main where


----------------------------------------------------------
-- Section 0: Imports.                                  --
----------------------------------------------------------
import           Control.Applicative                         ((<$>), (<*>))
import           Control.Concurrent.MVar                     (MVar, isEmptyMVar,
                                                              newEmptyMVar,
                                                              tryPutMVar,
                                                              tryTakeMVar)
import           Control.Lens
import           Control.Monad                               (when)
import           Data.ByteString                             (ByteString)
import qualified Data.Map                                    as M
import           Data.Maybe                                  (fromMaybe)
import           Data.Text                                   (Text)
import qualified Data.Text                                   as T
import qualified Data.Text.Encoding                          as T
import           Snap                                        (Handler,
                                                              Method (..),
                                                              Snaplet,
                                                              SnapletInit,
                                                              addRoutes,
                                                              getParam, liftIO,
                                                              makeSnaplet,
                                                              method,
                                                              nestSnaplet,
                                                              route, void, with,
                                                              writeBS,
                                                              writeText)
import qualified Snap
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Backends.CookieSession
import           System.Directory                            (doesFileExist,
                                                              removeFile)
import           System.IO
import           Text.Digestive

import           Test.Hspec
import           Test.Hspec.Snap

----------------------------------------------------------
-- Section 1: Example application used for testing.     --
----------------------------------------------------------
data App = App { _mv :: MVar (), _sess :: Snaplet SessionManager }

makeLenses ''App

instance HasSession App where
  getSessionLens = sess

html :: Text
html = "<table><tr><td>One</td><td>Two</td></tr></table>"

testForm :: Form Text (Handler App App) (Text, Text)
testForm = (,) <$> "a" .: check "Should not be empty" (\t -> not $ T.null t) (text Nothing)
               <*> "b" .: text Nothing


routes :: [(ByteString, Handler App App ())]
routes = [("/test", method GET $ writeText html)
         ,("/test", method POST $ writeText "")
         ,("/params", do mq <- getParam "q"
                         writeBS $ fromMaybe "" mq)
         ,("/redirect", Snap.redirect "/test")
         ,("/setmv", do m <- use mv
                        void $ liftIO $ tryPutMVar m ()
                        return ())
         ,("/setsess/:k", do Just k <- fmap T.decodeUtf8 <$> getParam "k"
                             with sess $ setInSession k "bar" >> commitSession
                             writeText "")
         ,("/getsess/:k", do Just k <- fmap T.decodeUtf8 <$> getParam "k"
                             Just r <- with sess $ getFromSession k
                             writeText r)
         ]

app :: MVar () -> SnapletInit App App
app mvar = makeSnaplet "app" "An snaplet example application." Nothing $ do
         addRoutes routes
         s <- nestSnaplet "sess" sess $ initCookieSessionManager "site_key.txt" "sess" (Just 3600)
         Snap.onUnload (do e <- doesFileExist "site_key.txt"
                           when e $ removeFile "site_key.txt")
         return (App mvar s)


----------------------------------------------------------
-- Section 2: Test suite against application.           --
----------------------------------------------------------

tests :: MVar () -> Spec
tests mvar =
  snap (route routes) (app mvar) $ do
    describe "requests" $ do
      it "should match selector from a GET request" $ do
        p <- get "/test"
        shouldHaveSelector "table td" p
        shouldNotHaveSelector "table td.doesntexist" p
        get "/redirect" >>= shouldNotHaveSelector "table td.doesntexist"
        get "/invalid_url" >>= shouldNotHaveSelector "table td.doesntexist"
      it "should not match <html> on POST request" $
        post "/test" M.empty >>= shouldNotHaveText "<html>"
      it "should post parameters" $ do
        post "/params" (params [("q", "hello")]) >>= shouldHaveText "hello"
        post "/params" (params [("r", "hello")]) >>= shouldNotHaveText "hello"
      it "basic equality" $ do
        eval (return 1) >>= shouldEqual 1
        shouldNotEqual 1 2
      it "status code 200" $ do
        get "/test" >>= should200
        get "/invalid_url" >>= shouldNot200
      it "status code 404" $ do
        get "/test" >>= shouldNot404
        get "/invalid_url" >>= should404
      it "status code 3**" $ do
        get "/redirect" >>= should300
        get "/test" >>= shouldNot300
      it "status code 3** with target" $ do
        get "/redirect" >>= should300To "/test"
        get "/redirect" >>= shouldNot300To "/redirect"
        get "/test" >>= shouldNot300To "/redirect"
    describe "stateful changes" $ do
      let isE = use mv >>= \m -> liftIO $ isEmptyMVar m
      after (\_ -> void $ tryTakeMVar mvar) $
        it "should reflect stateful in handler" $ do
         eval isE >>= shouldEqual True
         post "/setmv" M.empty
         eval isE >>= shouldEqual False
         post "/setmv" M.empty
         eval isE >>= shouldEqual False
         eval (use mv >>= \m -> void $ liftIO $ tryTakeMVar m)
      it "cleans up" $ eval isE >>= shouldEqual True
    describe "forms" $ do
      it "should pass valid data" $ do
        form (Value ("foo", "bar")) testForm (M.fromList [("a", "foo"), ("b", "bar")])
        form (Value ("foo", "")) testForm (M.fromList [("a", "foo")])
      it "should fail on invalid data" $ do
        form (ErrorPaths ["a"]) testForm (M.fromList [("a", ""), ("b", "bar")])
        form (ErrorPaths ["a"]) testForm (M.fromList [("b", "bar")])
        form (ErrorPaths ["a"]) testForm (M.fromList [])
      it "should call predicates on valid data" $ do
        form (Predicate (("oo" `T.isInfixOf`) . fst)) testForm (M.fromList [("a", "foobar")])
    describe "sessions" $ do
      it "should be able to modify session in handlers" $
        recordSession $ do get "/setsess/4"
                           sessionShouldContain "4"
                           sessionShouldContain "bar"
      it "should be able to modify session with eval" $
        recordSession $ do eval (with sess $ setInSession "foozlo" "bar" >> commitSession)
                           sessionShouldContain "foozlo"
                           sessionShouldContain "bar"
      it "should be able to persist sessions between requests" $
        recordSession $ do get "/setsess/3"
                           get "/getsess/3" >>= shouldHaveText "bar"
      it "should be able to persist sessions between eval and requests" $
        recordSession $ do eval (with sess $ setInSession "2" "bar" >> commitSession)
                           get "/getsess/2" >>= shouldHaveText "bar"
      it "should be able to persist sessions between requests and eval" $
        recordSession $ do get "/setsess/1"
                           eval (with sess $ getFromSession "1" ) >>= shouldEqual (Just "bar")
      it "should be able to persist sessions between eval and eval" $
        recordSession $ do eval (with sess $ setInSession "foofoo" "bar" >> commitSession)
                           eval (with sess $ getFromSession "foofoo" ) >>= shouldEqual (Just "bar")


----------------------------------------------------------
-- Section 3: Code to interface with cabal test.        --
----------------------------------------------------------
main :: IO ()
main = do
  mvar <- newEmptyMVar
  hspec (tests mvar)
