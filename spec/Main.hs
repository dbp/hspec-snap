{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}

module Main where


----------------------------------------------------------
-- Section 0: Imports.                                  --
----------------------------------------------------------
import           Control.Applicative     ((<$>), (<*>))
import           Control.Lens
import           Data.ByteString         (ByteString)
import qualified Data.Map                as M
import           Data.Maybe              (fromMaybe)
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Snap                    (Handler, Method (..), SnapletInit,
                                          addRoutes, getParam, liftIO,
                                          makeSnaplet, method, route, void,
                                          writeBS, writeText)
import qualified Snap
import           Text.Digestive

import           Control.Concurrent.MVar (MVar, isEmptyMVar, newEmptyMVar,
                                          tryPutMVar, tryTakeMVar)
import           Test.Hspec
import           Test.Hspec.Snap

----------------------------------------------------------
-- Section 1: Example application used for testing.     --
----------------------------------------------------------
data App = App { _mv :: MVar () }

makeLenses ''App

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
         ]

app :: MVar () -> SnapletInit App App
app mvar = makeSnaplet "app" "An snaplet example application." Nothing $ do
         addRoutes routes
         return (App mvar)


----------------------------------------------------------
-- Section 2: Test suite against application.           --
----------------------------------------------------------

tests :: MVar () -> Spec
tests mvar =
  snap (route routes) (app mvar) $ do
    describe "requests" $ do
      it "should match selector from a GET request" $ do
        p <- get "/test"
        p `shouldHaveSelector` "table td"
        p `shouldNotHaveSelector` "table td.doesntexist"
        get "/redirect" >>= flip shouldNotHaveSelector "table td.doesntexist"
        get "/invalid_url" >>= flip shouldNotHaveSelector "table td.doesntexist"
      it "should not match <html> on POST request" $
        post "/test" M.empty >>= flip shouldNotHaveText "<html>"
      it "should post parameters" $ do
        post "/params" (params [("q", "hello")]) >>= flip shouldHaveText "hello"
        post "/params" (params [("r", "hello")]) >>= flip shouldNotHaveText "hello"
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


----------------------------------------------------------
-- Section 3: Code to interface with cabal test.        --
----------------------------------------------------------
main :: IO ()
main = do
  mvar <- newEmptyMVar
  hspec (tests mvar)
