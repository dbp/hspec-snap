{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

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

  -- * General Hspec helpers
  , afterAll

  -- * Requests
  , get
  , get'
  , post
  , params

  -- * Helpers for dealing with TestResponses
  , restrictResponse

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
import           Control.Concurrent.MVar (modifyMVar, newEmptyMVar, newMVar,
                                          putMVar, takeMVar)
import           Control.Exception       (SomeException, catch)
import           Control.Monad           (void)
import           Control.Monad.State     (StateT (..), runStateT)
import qualified Control.Monad.State     as S (get, put)
import           Control.Monad.Trans     (liftIO)
import           Data.ByteString         (ByteString)
import qualified Data.Map                as M
import           Data.Maybe              (fromMaybe)
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import           Snap.Core               (Response (..), getHeader)
import qualified Snap.Core               as Snap
import           Snap.Snaplet            (Handler, Snaplet, SnapletInit)
import           Snap.Snaplet.Test       (InitializerState, closeSnaplet,
                                          evalHandler', getSnaplet, runHandler')
import           Snap.Test               (RequestBuilder, getResponseBody)
import qualified Snap.Test               as Test
import           Test.Hspec
import           Test.Hspec.Core
import qualified Text.Digestive          as DF
import qualified Text.HandsomeSoup       as HS
import qualified Text.XML.HXT.Core       as HXT

-- | The result of making requests against your application. Most
-- assertions act against these types (for example, `should200`,
-- `shouldHaveSelector`, etc).
data TestResponse = Html Text | NotFound | Redirect Int Text | Other Int | Empty deriving (Show, Eq)

-- | The main monad that tests run inside of. This allows both access
-- to the application (via requests and `eval`) and to running
-- assertions (like `should404` or `shouldHaveText`).
type SnapHspecM b = StateT (SnapHspecState b) IO

-- | Internal state used to share site initialization across tests, and to propogate failures.
data SnapHspecState b = SnapHspecState Result (Handler b b ()) (Snaplet b) (InitializerState b)


instance Example (SnapHspecM b ()) where
  type Arg (SnapHspecM b ()) = SnapHspecState b
  evaluateExample s _ cb _ =
    do mv <- newEmptyMVar
       cb $ \st@(SnapHspecState _ _ _ _) -> do ((),(SnapHspecState r' _ _ _)) <- runStateT s st
                                               putMVar mv r'
       takeMVar mv

-- | Runs a given action once after all the tests in the given block have run.
afterAll :: IO () -> SpecWith a -> SpecWith a
afterAll action = go
  where go spec = do forest <- runIO $ runSpecM spec
                     res <- runIO $ mapM countFlatten forest
                     let specs = map snd res
                     let count = foldr (+) 0 (map fst res)
                     mvar <- runIO $ newMVar count
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
        cleanup mv = modifyMVar mv $ \v -> if v == 1
                                              then action >>= return . (v,)
                                              else return (v - 1, ())

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
  case snapinit of
    Left err -> error $ show err
    Right (snaplet, initstate) -> do
      afterAll (closeSnaplet initstate) $
        before (return (SnapHspecState Success site snaplet initstate)) spec

-- | This allows you to change the default handler you are running
-- requests against within a block. This is most likely useful for
-- setting request state (for example, logging a user in).
modifySite :: (Handler b b () -> Handler b b ())
           -> SpecWith (SnapHspecState b)
           -> SpecWith (SnapHspecState b)
modifySite f = beforeWith (\(SnapHspecState r site snaplet initst) ->
                             return (SnapHspecState r (f site) snaplet initst))

-- | This performs a similar operation to `modifySite` but in the context
-- of `SnapHspecM` (which is needed if you need to `eval`, produce values, and
-- hand them somewhere else (so they can't be created within `f`).
modifySite' :: (Handler b b () -> Handler b b ())
            -> SnapHspecM b a
            -> SnapHspecM b a
modifySite' f a = do (SnapHspecState r site s i) <- S.get
                     S.put (SnapHspecState r (f site) s i)
                     a

-- | Evaluate a Handler action after each test.
afterEval :: Handler b b () -> SpecWith (SnapHspecState b) -> SpecWith (SnapHspecState b)
afterEval h = after (\(SnapHspecState r site s i) -> void $ evalHandlerSafe h s i)

-- | Evaluate a Handler action before each test.
beforeEval :: Handler b b () -> SpecWith (SnapHspecState b) -> SpecWith (SnapHspecState b)
beforeEval h = beforeWith (\state@(SnapHspecState r site s i) -> do void $ evalHandlerSafe h s i
                                                                    return state)

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

-- | Restricts a response to matches for a given CSS selector.
-- Does nothing to non-Html responses.
restrictResponse :: Text -> TestResponse -> TestResponse
restrictResponse selector (Html body) =
  case HXT.runLA (HXT.xshow $ HXT.hread HXT.>>> HS.css (T.unpack selector)) (T.unpack body) of
    [] -> Html ""
    matches -> Html (T.concat (map T.pack matches))
restrictResponse _ r = r

-- | Runs an arbitrary stateful action from your application.
eval :: Handler b b a -> SnapHspecM b a
eval act = do (SnapHspecState _ _ app is) <- S.get
              liftIO $ fmap (either (error . T.unpack) id) $ evalHandlerSafe act app is


-- | Records a test Success or Fail. Only the first Fail will be
-- recorded (and will cause the whole block to Fail).
setResult :: Result -> SnapHspecM b ()
setResult r = do (SnapHspecState r' s a i) <- S.get
                 case r' of
                   Success -> S.put (SnapHspecState r s a i)
                   _ -> return ()

-- | Asserts that a given stateful action will produce a specific different result after
-- an action has been run.
shouldChange :: (Show a, Eq a)
             => (a -> a)
             -> (Handler b b a)
             -> SnapHspecM b c
             -> SnapHspecM b ()
shouldChange f v act = do before' <- eval v
                          act
                          after' <- eval v
                          shouldEqual (f before') after'


-- | Asserts that two values are equal.
shouldEqual :: (Show a, Eq a)
            => a
            -> a
            -> SnapHspecM b ()
shouldEqual a b = if a == b
                      then setResult Success
                      else setResult (Fail ("Should have held: " ++ show a ++ " == " ++ show b))

-- | Asserts that two values are not equal.
shouldNotEqual :: (Show a, Eq a)
               => a
               -> a
               -> SnapHspecM b ()
shouldNotEqual a b = if a == b
                         then setResult (Fail ("Should not have held: " ++ show a ++ " == " ++ show b))
                         else setResult Success

-- | Asserts that the value is True.
shouldBeTrue :: Bool
             -> SnapHspecM b ()
shouldBeTrue True = setResult Success
shouldBeTrue False = setResult (Fail "Value should have been True.")

-- | Asserts that the value is not True (otherwise known as False).
shouldNotBeTrue :: Bool
                 -> SnapHspecM b ()
shouldNotBeTrue False = setResult Success
shouldNotBeTrue True = setResult (Fail "Value should have been True.")

-- | Asserts that the response is a success (either Html, or Other with status 200).
should200 :: TestResponse -> SnapHspecM b ()
should200 (Html _) = setResult Success
should200 (Other 200) = setResult Success
should200 r = setResult (Fail (show r))

-- | Asserts that the response is not a normal 200.
shouldNot200 :: TestResponse -> SnapHspecM b ()
shouldNot200 (Html _) = setResult (Fail "Got Html back.")
shouldNot200 (Other 200) = setResult (Fail "Got Other with 200 back.")
shouldNot200 _ = setResult Success

-- | Asserts that the response is a NotFound.
should404 :: TestResponse -> SnapHspecM b ()
should404 NotFound = setResult Success
should404 r = setResult (Fail (show r))

-- | Asserts that the response is not a NotFound.
shouldNot404 :: TestResponse -> SnapHspecM b ()
shouldNot404 NotFound = setResult (Fail "Got NotFound back.")
shouldNot404 _ = setResult Success

-- | Asserts that the response is a redirect.
should300 :: TestResponse -> SnapHspecM b ()
should300 (Redirect _ _) = setResult Success
should300 r = setResult (Fail (show r))

-- | Asserts that the response is not a redirect.
shouldNot300 :: TestResponse -> SnapHspecM b ()
shouldNot300 (Redirect _ _) = setResult (Fail "Got Redirect back.")
shouldNot300 _ = setResult Success

-- | Asserts that the response is a redirect, and thet the url it
-- redirects to starts with the given path.
should300To :: Text -> TestResponse -> SnapHspecM b ()
should300To pth (Redirect _ to) | pth `T.isPrefixOf` to = setResult Success
should300To _ r = setResult (Fail (show r))

-- | Asserts that the response is not a redirect to a given path. Note
-- that it can still be a redirect for this assertion to succeed, the
-- path it redirects to just can't start with the given path.
shouldNot300To :: Text -> TestResponse -> SnapHspecM b ()
shouldNot300To pth (Redirect _ to) | pth `T.isPrefixOf` to = setResult (Fail "Got Redirect back.")
shouldNot300To _ _ = setResult Success

-- | Assert that a response (which should be Html) has a given selector.
shouldHaveSelector :: Text -> TestResponse -> SnapHspecM b ()
shouldHaveSelector selector r@(Html body) =
  setResult $ if haveSelector' selector r
                then Success
                else (Fail msg)
  where msg = (T.unpack $ T.concat ["Html should have contained selector: ", selector, "\n\n", body])
shouldHaveSelector match _ = setResult (Fail (T.unpack $ T.concat ["Non-HTML body should have contained css selector: ", match]))

-- | Assert that a response (which should be Html) doesn't have a given selector.
shouldNotHaveSelector :: Text -> TestResponse -> SnapHspecM b ()
shouldNotHaveSelector selector r@(Html body) =
  setResult $ if haveSelector' selector r
                then (Fail msg)
                else Success
  where msg = (T.unpack $ T.concat ["Html should not have contained selector: ", selector, "\n\n", body])
shouldNotHaveSelector _ _ = setResult Success

haveSelector' :: Text -> TestResponse -> Bool
haveSelector' selector (Html body) =
  case HXT.runLA (HXT.hread HXT.>>> HS.css (T.unpack selector)) (T.unpack body)  of
    [] -> False
    _ -> True
haveSelector' _ _ = False

-- | Asserts that the response (which should be Html) contains the given text.
shouldHaveText :: Text -> TestResponse -> SnapHspecM b ()
shouldHaveText match (Html body) =
  if T.isInfixOf match body
  then setResult Success
  else setResult (Fail $ T.unpack $ T.concat [body, "' contains '", match, "'."])
shouldHaveText match _ = setResult (Fail (T.unpack $ T.concat ["Body contains: ", match]))

-- | Asserts that the response (which should be Html) does not contain the given text.
shouldNotHaveText :: Text -> TestResponse -> SnapHspecM b ()
shouldNotHaveText match (Html body) =
  if T.isInfixOf match body
  then setResult (Fail $ T.unpack $ T.concat [body, "' contains '", match, "'."])
  else setResult Success
shouldNotHaveText _ _ = setResult Success


-- | A data type for tests against forms.
data FormExpectations a = Value a           -- ^ The value the form should take (and should be valid)
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
       ErrorPaths expectedPaths ->
         do let viewErrorPaths = map (DF.fromPath . fst) $ DF.viewErrors $ fst r
            shouldBeTrue (all (`elem` viewErrorPaths) expectedPaths
                          && (length viewErrorPaths == length expectedPaths))
  where lookupParam pth = case M.lookup (DF.fromPath pth) fixedParams of
                            Nothing -> return []
                            Just v -> return [DF.TextInput v]
        fixedParams = M.mapKeys (T.append "form.") theParams

-- | Runs a request (built with helpers from Snap.Test), resulting in a response.
runRequest :: RequestBuilder IO () -> SnapHspecM b TestResponse
runRequest req = do
  (SnapHspecState _ site app is) <- S.get
  res <- liftIO $ runHandlerSafe req site app is
  case res of
    Left err -> do
      error $ T.unpack err
    Right response -> do
      case rspStatus response of
        404 -> return NotFound
        200 -> do
          body <- liftIO $ getResponseBody response
          return $ Html $ T.decodeUtf8 body
        _ -> if (rspStatus response) >= 300 && (rspStatus response) < 400
                then do let url = fromMaybe "" $ getHeader "Location" response
                        return (Redirect (rspStatus response) (T.decodeUtf8 url))
                else return (Other (rspStatus response))

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
