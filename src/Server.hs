module Server where

import           ClassyPrelude
import           Web.Spock
import           Web.Spock.Config
import           Avtor
import           Control.Monad.Trans
import           Data.Monoid
import           Views
import           Lucid.Base

lucid :: MonadIO m => Html a -> ActionCtxT cxt m b
lucid x = do
  setHeader "Content-Type" "text/html; charset=utf-8"
  lazyBytes (renderBS x)

-- lucid_ :: MonadIO m => Html a -> ActionCtxT cxt m b
-- lucid_ x = html $ toStrict $ renderText x

data MySess  = EmptySession
data MyState = DummyAppState (IORef Int)

-- Route Strings

signInR  = "/signin"
signUpR  = "/signup"
signOutR = "/signout"
verifyR  = "/verify"

app :: IO ()
app = do
  ref      <- newIORef 0
  spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
  runSpock 8080 (spock spockCfg routes)

findAccountByName' :: Text -> IO (Maybe Account)
findAccountByName' name = do
  putStrLn $ "Account being called " <> name
  return $
    Nothing

insertAccount' :: Account -> IO (Either AvtorError ())
insertAccount' account = do
  putStrLn "blah"
  return $
    Right ()

routes :: SpockM () MySess MyState ()
routes = do
  get root $
    lucid signInFormView
  get signInR $
    lucid signInFormView
  post signInR $
    text "You Signed In"
  get signOutR $
    text "You Signed Out"
  get signUpR $
    lucid signUpFormView
  post signUpR $
    text "You Signed Up"
  get (verifyR <//> var) $ \token ->
    text token