{-# LANGUAGE DataKinds                                                     #-}
{-# LANGUAGE FlexibleContexts                                              #-}
{-# LANGUAGE FlexibleInstances                                             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving                                    #-}
{-# LANGUAGE FunctionalDependencies                                        #-}
{-# LANGUAGE MultiParamTypeClasses                                         #-}
{-# LANGUAGE OverloadedStrings                                             #-}
{-# LANGUAGE ScopedTypeVariables                                           #-}
{-# LANGUAGE TupleSections                                                 #-}
{-# LANGUAGE TypeFamilies                                                  #-}
{-# LANGUAGE TypeSynonymInstances                                          #-}

module Test.Hspec.Snap (
  -- * Running blocks of hspec-snap tests
    snap
  , modifySite
  , modifySite'
  , afterEval
  , beforeEval

  -- * Core data types
  , TestResponse(..)
  , SnapHspecM

  -- * Factory style test data generation
  , Factory(..)

  -- * Requests
  , delete
  , get
  , get'
  , post
  , postJson
  , put
  , put'
  , params

  -- * Helpers for dealing with TestResponses
  , restrictResponse

  -- * Dealing with session state (EXPERIMENTAL)
  , recordSession
  , HasSession(..)
  , sessionShouldContain
  , sessionShouldNotContain

  -- * Evaluating application code
  , eval

  -- * Unit test assertions
  , shouldChange
  , shouldEqual
  , shouldNotEqual
  , shouldBeTrue
  , shouldNotBeTrue

  -- * Response assertions
  , should200
  , shouldNot200
  , should404
  , shouldNot404
  , should300
  , shouldNot300
  , should300To
  , shouldNot300To
  , shouldHaveSelector
  , shouldNotHaveSelector
  , shouldHaveText
  , shouldNotHaveText

  -- * Form tests
  , FormExpectations(..)
  , form


  -- * Internal types and helpers
  , SnapHspecState(..)
  , setResult
  , runRequest
  , runHandlerSafe
  , evalHandlerSafe
  ) where

import           Control.Applicative     ((<$>))
import           Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar,
                                          readMVar, takeMVar)

import           Control.Exception       (SomeException, catch)
import           Control.Monad           (void)
import           Control.Monad.State     (StateT (..), runStateT)
import qualified Control.Monad.State     as S (get, put)
import           Control.Monad.Trans     (liftIO)
import           Data.Aeson              (ToJSON, encode)
import           Data.ByteString         (ByteString)
import           Data.ByteString.Lazy    (fromStrict, toStrict)
import qualified Data.ByteString.Lazy    as LBS (ByteString)
import qualified Data.Map                as M
import           Data.Maybe              (fromMaybe)
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import           Snap.Core               (Response (..), getHeader)
import qualified Snap.Core               as Snap
import           Snap.Snaplet            (Handler, Snaplet, SnapletInit,
                                          SnapletLens, with)
import           Snap.Snaplet.Session    (SessionManager, commitSession,
                                          sessionToList, setInSession)
import           Snap.Snaplet.Test       (InitializerState, closeSnaplet,
                                          evalHandler', getSnaplet, runHandler')
import           Snap.Test               (RequestBuilder, getResponseBody)
import qualified Snap.Test               as Test
import           Test.Hspec
import           Test.Hspec.Core.Spec
import qualified Text.Digestive          as DF
import qualified Text.HandsomeSoup       as HS
import qualified Text.XML.HXT.Core       as HXT

-- derives Num and Ord to avoid excessive newtype wrapping and unwrapping
-- in pattern matching, etc.
newtype RespCode = RespCode Int deriving (Show, Read, Eq, Num, Ord)

-- | The result of making requests against your application. Most
-- assertions act against these types (for example, `should200`,
-- `shouldHaveSelector`, etc).
data TestResponse = Html RespCode Text
                  | Json RespCode LBS.ByteString
                  | NotFound
                  | Redirect RespCode Text
                  | Other RespCode
                  | Empty
                  deriving (Show, Eq)

-- | The main monad that tests run inside of. This allows both access
-- to the application (via requests and `eval`) and to running
-- assertions (like `should404` or `shouldHaveText`).
type SnapHspecM b = StateT (SnapHspecState b) IO

-- | Internal state used to share site initialization across tests, and to propogate failures.
-- Understanding it is completely unnecessary to use the library.
--
-- The fields it contains, in order, are:
--
-- > Result
-- > Main handler
-- > Startup state
-- > Startup state
-- > Session state
-- > Before handler (runs before each eval)
-- > After handler (runs after each eval).
data SnapHspecState b = SnapHspecState Result
                                       (Handler b b ())
                                       (Snaplet b)
                                       (InitializerState b)
                                       (MVar [(Text, Text)])
                                       (Handler b b ())
                                       (Handler b b ())


instance Example (SnapHspecM b ()) where
  type Arg (SnapHspecM b ()) = SnapHspecState b
  evaluateExample s _ cb _ =
    do mv <- newEmptyMVar
       cb $ \st -> do ((),SnapHspecState r' _ _ _ _ _ _) <- runStateT s st
                      putMVar mv r'
       takeMVar mv

-- | Factory instances allow you to easily generate test data.
--
-- Essentially, you specify a default way of constructing a
-- data type, and allow certain parts of it to be modified (via
-- the 'fields' data structure).
--
-- An example follows:
--
-- > data Foo = Foo Int
-- > newtype FooFields = FooFields (IO Int)
-- > instance Factory App Foo FooFields where
-- >   fields = FooFields randomIO
-- >   save f = liftIO f >>= saveFoo . Foo1
-- >
-- > main = do create id :: SnapHspecM App Foo
-- >           create (const $ FooFields (return 1)) :: SnapHspecM App Foo
class Factory b a d | a -> b, a -> d, d -> a where
  fields :: d
  save :: d -> SnapHspecM b a
  create :: (d -> d) -> SnapHspecM b a
  create transform = save $ transform fields
  reload :: a -> SnapHspecM b a
  reload = return


-- | The way to run a block of `SnapHspecM` tests within an `hspec`
-- test suite. This takes both the top level handler (usually `route
-- routes`, where `routes` are all the routes for your site) and the
-- site initializer (often named `app`), and a block of tests. A test
-- suite can have multiple calls to `snap`, though each one will cause
-- the site initializer to run, which is often a slow operation (and
-- will slow down test suites).
snap :: Handler b b () -> SnapletInit b b -> SpecWith (SnapHspecState b) -> Spec
snap site app spec = do
  snapinit <- runIO $ getSnaplet (Just "test") app
  mv <- runIO (newMVar [])
  case snapinit of
    Left err -> error $ show err
    Right (snaplet, initstate) ->
      afterAll (const $ closeSnaplet initstate) $
        before (return (SnapHspecState Success site snaplet initstate mv (return ()) (return ()))) spec

-- | This allows you to change the default handler you are running
-- requests against within a block. This is most likely useful for
-- setting request state (for example, logging a user in).
modifySite :: (Handler b b () -> Handler b b ())
           -> SpecWith (SnapHspecState b)
           -> SpecWith (SnapHspecState b)
modifySite f = beforeWith (\(SnapHspecState r site snaplet initst sess bef aft) ->
                             return (SnapHspecState r (f site) snaplet initst sess bef aft))

-- | This performs a similar operation to `modifySite` but in the context
-- of `SnapHspecM` (which is needed if you need to `eval`, produce values, and
-- hand them somewhere else (so they can't be created within `f`).
modifySite' :: (Handler b b () -> Handler b b ())
            -> SnapHspecM b a
            -> SnapHspecM b a
modifySite' f a = do (SnapHspecState r site s i sess bef aft) <- S.get
                     S.put (SnapHspecState r (f site) s i sess bef aft)
                     a

-- | Evaluate a Handler action after each test.
afterEval :: Handler b b () -> SpecWith (SnapHspecState b) -> SpecWith (SnapHspecState b)
afterEval h = after (\(SnapHspecState _r _site s i _ _ _) ->
                       do res <- evalHandlerSafe h s i
                          case res of
                            Right _ -> return ()
                            Left msg -> liftIO $ print msg)

-- | Evaluate a Handler action before each test.
beforeEval :: Handler b b () -> SpecWith (SnapHspecState b) -> SpecWith (SnapHspecState b)
beforeEval h = beforeWith (\state@(SnapHspecState _r _site s i _ _ _) -> do void $ evalHandlerSafe h s i
                                                                            return state)

class HasSession b where
  getSessionLens :: SnapletLens b SessionManager

recordSession :: HasSession b => SnapHspecM b a -> SnapHspecM b a
recordSession a =
  do (SnapHspecState r site s i mv bef aft) <- S.get
     S.put (SnapHspecState r site s i mv
                             (do ps <- liftIO $ readMVar mv
                                 with getSessionLens $ mapM_ (uncurry setInSession) ps
                                 with getSessionLens commitSession)
                             (do ps' <- with getSessionLens sessionToList
                                 void . liftIO $ takeMVar mv
                                 liftIO $ putMVar mv ps'))
     res <- a
     (SnapHspecState r' _ _ _ _ _ _) <- S.get
     void . liftIO $ takeMVar mv
     liftIO $ putMVar mv []
     S.put (SnapHspecState r' site s i mv bef aft)
     return res

sessContents :: SnapHspecM b Text
sessContents = do
  (SnapHspecState _ _ _ _ mv _ _) <- S.get
  ps <- liftIO $ readMVar mv
  return $ T.concat (map (uncurry T.append) ps)

sessionShouldContain :: Text -> SnapHspecM b ()
sessionShouldContain t =
  do contents <- sessContents
     if t `T.isInfixOf` contents
       then setResult Success
       else setResult (Fail Nothing $ "Session did not contain: " ++ T.unpack t
                                    ++ "\n\nSession was:\n" ++ T.unpack contents)

sessionShouldNotContain :: Text -> SnapHspecM b ()
sessionShouldNotContain t =
  do contents <- sessContents
     if t `T.isInfixOf` contents
       then setResult (Fail Nothing $ "Session should not have contained: " ++ T.unpack t
                                    ++ "\n\nSession was:\n" ++ T.unpack contents)
       else setResult Success

-- | Runs a DELETE request
delete :: Text -> SnapHspecM b TestResponse
delete path = runRequest (Test.delete (T.encodeUtf8 path) M.empty)

-- | Runs a GET request.
get :: Text -> SnapHspecM b TestResponse
get path = get' path M.empty

-- | Runs a GET request, with a set of parameters.
get' :: Text -> Snap.Params -> SnapHspecM b TestResponse
get' path ps = runRequest (Test.get (T.encodeUtf8 path) ps)

-- | A helper to construct parameters.
params :: [(ByteString, ByteString)] -- ^ Pairs of parameter and value.
       -> Snap.Params
params = M.fromList . map (\x -> (fst x, [snd x]))

-- | Creates a new POST request, with a set of parameters.
post :: Text -> Snap.Params -> SnapHspecM b TestResponse
post path ps = runRequest (Test.postUrlEncoded (T.encodeUtf8 path) ps)

-- | Creates a new POST request with a given JSON value as the request body.
postJson :: ToJSON tj => Text -> tj -> SnapHspecM b TestResponse
postJson path json = runRequest $ Test.postRaw (T.encodeUtf8 path)
                                               "application/json"
                                               (toStrict $ encode json)

-- | Creates a new PUT request, with a set of parameters, with a default type of "application/x-www-form-urlencoded"
put :: Text -> Snap.Params -> SnapHspecM b TestResponse
put path params' = put' path "application/x-www-form-urlencoded" params'

-- | Creates a new PUT request with a configurable MIME/type
put' :: Text -> Text -> Snap.Params -> SnapHspecM b TestResponse
put' path mime params' = runRequest $ do
  Test.put (T.encodeUtf8 path) (T.encodeUtf8 mime) ""
  Test.setQueryString params'

-- | Restricts a response to matches for a given CSS selector.
-- Does nothing to non-Html responses.
restrictResponse :: Text -> TestResponse -> TestResponse
restrictResponse selector (Html code body) =
  case HXT.runLA (HXT.xshow $ HXT.hread HXT.>>> HS.css (T.unpack selector)) (T.unpack body) of
    [] -> Html code ""
    matches -> Html code (T.concat (map T.pack matches))
restrictResponse _ r = r

-- | Runs an arbitrary stateful action from your application.
eval :: Handler b b a -> SnapHspecM b a
eval act = do (SnapHspecState _ _site app is _mv bef aft) <- S.get
              liftIO $ either (error . T.unpack) id <$> evalHandlerSafe (do bef
                                                                            r <- act
                                                                            aft
                                                                            return r) app is


-- | Records a test Success or Fail. Only the first Fail will be
-- recorded (and will cause the whole block to Fail).
setResult :: Result -> SnapHspecM b ()
setResult r = do (SnapHspecState r' s a i sess bef aft) <- S.get
                 case r' of
                   Success -> S.put (SnapHspecState r s a i sess bef aft)
                   _ -> return ()

-- | Asserts that a given stateful action will produce a specific different result after
-- an action has been run.
shouldChange :: (Show a, Eq a)
             => (a -> a)
             -> Handler b b a
             -> SnapHspecM b c
             -> SnapHspecM b ()
shouldChange f v act = do before' <- eval v
                          void act
                          after' <- eval v
                          shouldEqual (f before') after'


-- | Asserts that two values are equal.
shouldEqual :: (Show a, Eq a)
            => a
            -> a
            -> SnapHspecM b ()
shouldEqual a b = if a == b
                      then setResult Success
                      else setResult (Fail Nothing ("Should have held: " ++ show a ++ " == " ++ show b))

-- | Asserts that two values are not equal.
shouldNotEqual :: (Show a, Eq a)
               => a
               -> a
               -> SnapHspecM b ()
shouldNotEqual a b = if a == b
                         then setResult (Fail Nothing ("Should not have held: " ++ show a ++ " == " ++ show b))
                         else setResult Success

-- | Asserts that the value is True.
shouldBeTrue :: Bool
             -> SnapHspecM b ()
shouldBeTrue True = setResult Success
shouldBeTrue False = setResult (Fail Nothing "Value should have been True.")

-- | Asserts that the value is not True (otherwise known as False).
shouldNotBeTrue :: Bool
                 -> SnapHspecM b ()
shouldNotBeTrue False = setResult Success
shouldNotBeTrue True = setResult (Fail Nothing "Value should have been True.")

-- | Asserts that the response is a success (either Html, or Other with status 200).
should200 :: TestResponse -> SnapHspecM b ()
should200 (Html _ _)   = setResult Success
should200 (Json 200 _) = setResult Success
should200 (Other 200)  = setResult Success
should200 r = setResult (Fail Nothing (show r))

-- | Asserts that the response is not a normal 200.
shouldNot200 :: TestResponse -> SnapHspecM b ()
shouldNot200 (Html _ _) = setResult (Fail Nothing "Got Html back.")
shouldNot200 (Other 200) = setResult (Fail Nothing "Got Other with 200 back.")
shouldNot200 _ = setResult Success

-- | Asserts that the response is a NotFound.
should404 :: TestResponse -> SnapHspecM b ()
should404 NotFound = setResult Success
should404 r = setResult (Fail Nothing (show r))

-- | Asserts that the response is not a NotFound.
shouldNot404 :: TestResponse -> SnapHspecM b ()
shouldNot404 NotFound = setResult (Fail Nothing "Got NotFound back.")
shouldNot404 _ = setResult Success

-- | Asserts that the response is a redirect.
should300 :: TestResponse -> SnapHspecM b ()
should300 (Redirect _ _) = setResult Success
should300 r = setResult (Fail Nothing (show r))

-- | Asserts that the response is not a redirect.
shouldNot300 :: TestResponse -> SnapHspecM b ()
shouldNot300 (Redirect _ _) = setResult (Fail Nothing "Got Redirect back.")
shouldNot300 _ = setResult Success

-- | Asserts that the response is a redirect, and thet the url it
-- redirects to starts with the given path.
should300To :: Text -> TestResponse -> SnapHspecM b ()
should300To pth (Redirect _ to) | pth `T.isPrefixOf` to = setResult Success
should300To _ r = setResult (Fail Nothing (show r))

-- | Asserts that the response is not a redirect to a given path. Note
-- that it can still be a redirect for this assertion to succeed, the
-- path it redirects to just can't start with the given path.
shouldNot300To :: Text -> TestResponse -> SnapHspecM b ()
shouldNot300To pth (Redirect _ to) | pth `T.isPrefixOf` to = setResult (Fail Nothing "Got Redirect back.")
shouldNot300To _ _ = setResult Success

-- | Assert that a response (which should be Html) has a given selector.
shouldHaveSelector :: Text -> TestResponse -> SnapHspecM b ()
shouldHaveSelector selector r@(Html _ body) =
  setResult $ if haveSelector' selector r
                then Success
                else Fail Nothing msg
  where msg = T.unpack $ T.concat ["Html should have contained selector: ", selector, "\n\n", body]
shouldHaveSelector match _ = setResult (Fail Nothing (T.unpack $ T.concat ["Non-HTML body should have contained css selector: ", match]))

-- | Assert that a response (which should be Html) doesn't have a given selector.
shouldNotHaveSelector :: Text -> TestResponse -> SnapHspecM b ()
shouldNotHaveSelector selector r@(Html _ body) =
  setResult $ if haveSelector' selector r
                then Fail Nothing msg
                else Success
  where msg = T.unpack $ T.concat ["Html should not have contained selector: ", selector, "\n\n", body]
shouldNotHaveSelector _ _ = setResult Success

haveSelector' :: Text -> TestResponse -> Bool
haveSelector' selector (Html _ body) =
  case HXT.runLA (HXT.hread HXT.>>> HS.css (T.unpack selector)) (T.unpack body)  of
    [] -> False
    _ -> True
haveSelector' _ _ = False

-- | Asserts that the response (which should be Html) contains the given text.
shouldHaveText :: Text -> TestResponse -> SnapHspecM b ()
shouldHaveText match (Html _ body) =
  if T.isInfixOf match body
  then setResult Success
  else setResult (Fail Nothing $ T.unpack $ T.concat [body, "' contains '", match, "'."])
shouldHaveText match _ = setResult (Fail Nothing (T.unpack $ T.concat ["Body contains: ", match]))

-- | Asserts that the response (which should be Html) does not contain the given text.
shouldNotHaveText :: Text -> TestResponse -> SnapHspecM b ()
shouldNotHaveText match (Html _ body) =
  if T.isInfixOf match body
  then setResult (Fail Nothing $ T.unpack $ T.concat [body, "' contains '", match, "'."])
  else setResult Success
shouldNotHaveText _ _ = setResult Success


-- | A data type for tests against forms.
data FormExpectations a = Value a           -- ^ The value the form should take (and should be valid)
                        | Predicate (a -> Bool)
                        | ErrorPaths [Text] -- ^ The error paths that should be populated

-- | Tests against digestive-functors forms.
form :: (Eq a, Show a)
     => FormExpectations a           -- ^ If the form should succeed, Value a is what it should produce.
                                     --   If failing, ErrorPaths should be all the errors that are triggered.
     -> DF.Form Text (Handler b b) a -- ^ The form to run
     -> M.Map Text Text                -- ^ The parameters to pass
     -> SnapHspecM b ()
form expected theForm theParams =
  do r <- eval $ DF.postForm "form" theForm (const $ return lookupParam)
     case expected of
       Value a -> shouldEqual (snd r) (Just a)
       Predicate f ->
         case snd r of
           Nothing -> setResult (Fail Nothing $ T.unpack $
                                 T.append "Expected form to validate. Resulted in errors: "
                                          (T.pack (show $ DF.viewErrors $ fst r)))
           Just v -> if f v
                       then setResult Success
                       else setResult (Fail Nothing $ T.unpack $
                                       T.append "Expected predicate to pass on value: "
                                                (T.pack (show v)))
       ErrorPaths expectedPaths ->
         do let viewErrorPaths = map (DF.fromPath . fst) $ DF.viewErrors $ fst r
            if all (`elem` viewErrorPaths) expectedPaths
               then if length viewErrorPaths == length expectedPaths
                       then setResult Success
                       else setResult (Fail Nothing $ "Number of errors did not match test. Got:\n\n "
                                            ++ show viewErrorPaths
                                            ++ "\n\nBut expected:\n\n"
                                            ++ show expectedPaths)
               else setResult (Fail Nothing $ "Did not have all errors specified. Got:\n\n"
                                    ++ show viewErrorPaths
                                    ++ "\n\nBut expected:\n\n"
                                    ++ show expectedPaths)
  where lookupParam pth = case M.lookup (DF.fromPath pth) fixedParams of
                            Nothing -> return []
                            Just v -> return [DF.TextInput v]
        fixedParams = M.mapKeys (T.append "form.") theParams

-- | Runs a request (built with helpers from Snap.Test), resulting in a response.
runRequest :: RequestBuilder IO () -> SnapHspecM b TestResponse
runRequest req = do
  (SnapHspecState _ site app is _ bef aft) <- S.get
  res <- liftIO $ runHandlerSafe req (bef >> site >> aft) app is
  case res of
    Left err ->
      error $ T.unpack err
    Right response -> let respCode = respStatus response in
      case respCode of
        404 -> return NotFound
        200 ->
          liftIO $ parse200 response
        _ -> if respCode >= 300 && respCode < 400
                then do let url = fromMaybe "" $ getHeader "Location" response
                        return (Redirect respCode (T.decodeUtf8 url))
                else return (Other respCode)

respStatus :: Response -> RespCode
respStatus = RespCode . rspStatus


parse200 :: Response -> IO TestResponse
parse200 resp =
    let body        = getResponseBody resp
        contentType = getHeader "content-type" resp in
    case contentType of
      Just "application/json" -> Json 200 . fromStrict <$> body
      _                       -> Html 200 . T.decodeUtf8 <$> body

-- | Runs a request against a given handler (often the whole site),
-- with the given state. Returns any triggered exception, or the response.
runHandlerSafe :: RequestBuilder IO ()
               -> Handler b b v
               -> Snaplet b
               -> InitializerState b
               -> IO (Either Text Response)
runHandlerSafe req site s is =
  catch (runHandler' s is req site) (\(e::SomeException) -> return $ Left (T.pack $ show e))

-- | Evaluates a given handler with the given state. Returns any
-- triggered exception, or the value produced.
evalHandlerSafe :: Handler b b v
                -> Snaplet b
                -> InitializerState b
                -> IO (Either Text v)
evalHandlerSafe act s is =
  catch (evalHandler' s is (Test.get "" M.empty) act) (\(e::SomeException) -> return $ Left (T.pack $ show e))

{-# ANN put ("HLint: ignore Eta reduce"::String)                            #-}
