{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
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
                                                              newMVar, putMVar,
                                                              takeMVar,
                                                              tryPutMVar,
                                                              tryTakeMVar)
import           Control.Lens                         hiding ((.=))
import           Control.Monad                               (void, when)
import           Control.Monad.IO.Class                      (liftIO)
import           Data.Aeson                                  (Value(..), (.=)
                                                             ,object, decode
                                                             ,ToJSON, FromJSON
                                                             ,toJSON, parseJSON)
import qualified Data.Aeson                                  as Ae ((.:))
import           Data.ByteString                             (ByteString)
import qualified Data.ByteString                             as BS (concat)
import           Data.Map                                    (Map)
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
                                                              getParam,
                                                              makeSnaplet,
                                                              method,
                                                              nestSnaplet,
                                                              route, with,
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

import           Utils                                       (writeJSON
                                                             ,parseJsonBody)

----------------------------------------------------------
-- Section 1: Example application used for testing.     --
----------------------------------------------------------
data Foo = Foo Int String String
data App = App { _mv :: MVar (), _store :: MVar (Map Int Foo), _sess :: Snaplet SessionManager }

makeLenses ''App

newFoo :: String -> String -> Handler App App Foo
newFoo s1 s2 = do smvar <- use store
                  mp <- liftIO $ takeMVar smvar
                  let i = 1 + M.size mp
                  let foo = Foo i s1 s2
                  liftIO $ putMVar smvar (M.insert i foo mp)
                  return foo

lookupFoo :: Int -> Handler App App (Maybe Foo)
lookupFoo i = do smvar <- use store
                 mp <- liftIO $ takeMVar smvar
                 liftIO $ putMVar smvar mp
                 return (M.lookup i mp)

instance HasSession App where
  getSessionLens = sess

html :: Text
html = "<html><table><tr><td>One</td><td>Two</td></tr></table></html>"

testForm :: Form Text (Handler App App) (Text, Text)
testForm = (,) <$> "a" .: check "Should not be empty" (\t -> not $ T.null t) (text Nothing)
               <*> "b" .: text Nothing

data ExampleObject = ExampleObject Integer Text deriving (Show, Eq)

instance ToJSON ExampleObject where
    toJSON (ExampleObject i t) = object [ "aNumber" .= i
                                        , "aString" .= t
                                        ]

instance FromJSON ExampleObject where
    parseJSON (Object o) = ExampleObject <$> o Ae..: "aNumber" <*>
                                             o Ae..: "aString"
    parseJSON _          = fail $ "Expected ExampleObject as JSON object"

exampleObj :: ExampleObject
exampleObj = ExampleObject 42 "foo"

writeParamsAndMethod :: Maybe ByteString -> Snap.Method -> Handler App App ()
writeParamsAndMethod mq m = case m of
                              POST -> writeBS $ methodAndParam "POST "
                              PUT  -> writeBS $ methodAndParam "PUT "
                              _    -> writeBS "Not valid"
  where
    methodAndParam p = BS.concat [p, fromMaybe "" mq]

routes :: [(ByteString, Handler App App ())]
routes = [("/test", method GET $ writeText html)
         ,("/test", method POST $ writeText "")
         ,("/test", method DELETE $ writeText "deleted")
         ,("/test", method PUT $ writeText "")
         ,("/params", do mq <- getParam "q"
                         writeParamsAndMethod mq =<< (Snap.rqMethod <$> Snap.getRequest))
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
         ,("/json", writeJSON $ exampleObj)
         ,("/postJson", do
                          Just (ExampleObject i t) <- parseJsonBody
                          writeJSON $ ExampleObject (i + 1)
                                                    (t `T.append` "!")
                          )
         ]

app :: MVar (Map Int Foo) -> MVar () -> SnapletInit App App
app state mvar = makeSnaplet "app" "An snaplet example application." Nothing $ do
   addRoutes routes
   s <- nestSnaplet "sess" sess $ initCookieSessionManager "site_key.txt" "sess" (Just 3600)
   Snap.onUnload (do e <- doesFileExist "site_key.txt"
                     when e $ removeFile "site_key.txt")
   return (App mvar state s)


----------------------------------------------------------
-- Section 2: Test suite against application.           --
----------------------------------------------------------

newtype FooFields = FooFields (IO String)

instance Factory App Foo FooFields where
  fields = FooFields (return "default")
  save (FooFields as) = do s <- liftIO as
                           eval (newFoo s "const")

tests :: MVar (Map Int Foo) -> MVar () -> Spec
tests store mvar =
  snap (route routes) (app store mvar) $ do
    describe "requests" $ do
      it "should match selector from a GET request" $ do
        p <- get "/test"
        shouldHaveSelector "table td" p
        shouldNotHaveSelector "table td.doesntexist" p
        get "/redirect" >>= shouldNotHaveSelector "table td.doesntexist"
        get "/invalid_url" >>= shouldNotHaveSelector "table td.doesntexist"
      it "should have deleted as text on the response" $
        delete "/test" >>= shouldHaveText "deleted"
      it "should not match <html> on POST request" $
        post "/test" M.empty >>= shouldNotHaveText "<html>"
      it "should post parameters" $ do
        post "/params" (params [("q", "hello")]) >>= shouldHaveText "POST hello"
        post "/params" (params [("r", "hello")]) >>= shouldNotHaveText "hello"
      it "should post json" $ do
        Json raw <- postJson "/postJson" exampleObj
        Just (ExampleObject 43 "foo!") `shouldEqual` decode raw
      it "should not match <html> on PUT request" $
        put "/test" M.empty >>= shouldNotHaveText "<html>"
      it "should put parameters" $ do
        put "/params" (params [("q", "hello")]) >>= shouldHaveText "PUT hello"
        put "/params" (params [("r", "hello")]) >>= shouldNotHaveText "hello"
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
      it "differentiates between response content types" $ do
        Json raw <- get "/json"
        Just exampleObj `shouldEqual` decode raw
        Html doc <- get "/test"
        doc `shouldEqual` html
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
      it "should be able to remove stuff from session" $
        recordSession $ do eval (with sess $ setInSession "foobar" "baz" >> commitSession)
                           sessionShouldContain "foobar"
                           eval (with sess $ deleteFromSession "foobar" >> commitSession)
                           sessionShouldNotContain "foobar"
    describe "factories" $ do
      it "should be able to generate a foo" $
        do (Foo i _ _) <- create id
           Just (Foo _ _ s) <- eval (lookupFoo i)
           s `shouldEqual` "const"
      it "should be able to modify defaulted values" $
        do (Foo _ s _) <- create (\_ -> FooFields (return "Hi!"))
           s `shouldEqual` "Hi!"
           (Foo _ s _) <- create id
           s `shouldNotEqual` "Hi!"


----------------------------------------------------------
-- Section 3: Code to interface with cabal test.        --
----------------------------------------------------------
main :: IO ()
main = do
  mvar <- newEmptyMVar
  store <- newMVar M.empty
  hspec (tests store mvar)
