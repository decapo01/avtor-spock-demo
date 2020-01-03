{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module PostgresComponents where

import ClassyPrelude
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import Data.ByteString
import Data.Time
import Data.Has
import Data.Pool
import Data.UUID.V4
import Data.List
import Data.UUID

import Avtor



type State = Pool Connection

type PG r m = (Has State r, MonadReader r m, MonadIO m)

data Config = Config
  { configUrl              :: ByteString
  , configStripeCount      :: Int
  , configMaxOpenPerStripe :: Int
  , configIdleConnTimeout  :: NominalDiffTime
  }


withPool :: Config -> (State -> IO a) -> IO a
withPool config action =
  bracket initPool cleanPool action
  where
    initPool  = createPool openConn closeConn (configStripeCount config) (configIdleConnTimeout config) (configMaxOpenPerStripe config)
    openConn  = connectPostgreSQL (configUrl config)
    closeConn = close
    cleanPool = destroyAllResources

findUserByEmailQuery = "select * from users where email = ? limit 1"

data UserRow
  = UserRow
  { name  :: String
  , email :: String
  }

instance ToField UserId where
  toField uid = toField $ _userId uid

instance FromField UserId where
  fromField field bs = UserId <$> fromField field bs

instance ToField AccountId where
  toField aid = toField $ _accountId aid

instance FromField AccountId where
  fromField field bs = AccountId <$> fromField field bs

instance ToField VerToken where
  toField v = toField $ verToken v

instance FromField VerToken where
  fromField field bs = VerToken <$> fromField field bs

-- instance FromField VeificationToken

instance FromRow Account where
  fromRow = Account <$> field <*> field

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field

instance ToRow User where
  toRow u = 
    [ toField (userId        u)
    , toField (userEmail     u)
    , toField (userPass      u)
    , toField (userAccountId u)
    ]

instance FromRow UserRow where
  fromRow = UserRow <$> field <*> field

instance FromRow UnverifiedUser where
  fromRow = UnverifiedUser <$> field <*> field <*> field <*> field <*> field


instance ToRow UnverifiedUser where
  toRow u =
    [ toField (unverifiedUserId        u)
    , toField (unverifiedUserEmail     u)
    , toField (unverifiedUserPassword  u)
    , toField (unverifiedUserToken     u)
    , toField (unverifiedUserAccountId u)
    ]

findUserByEmail :: Connection -> Text -> IO (Maybe User)
findUserByEmail    connection    email = do
  results <- query connection findUserByEmailQuery (Only email) :: IO [User]
  return $ maybeGet results 0

insertUser :: Connection -> User -> IO ()
insertUser connection user = do
  execute connection sql $ user
  return ()
  where
    sql = "insert into users (id, email, password, account_id) values (?,?,?,?)"

generateUuid :: () -> IO UUID
generateUuid _ = do
  uuid <- nextRandom
  return uuid


insertUnverifiedUser :: Connection -> UnverifiedUser -> IO (Either AvtorError Text)
insertUnverifiedUser connection unverifiedUser = do
  execute connection sql $ unverifiedUser
  return $ Right "Success"
  where
    sql = "insert into unverified_users (id,email,password,account_id, verification_token) values (?,?,?,?,?)"

findUnverifiedUserByEmail :: Connection -> Text -> IO (Either AvtorError (Maybe UnverifiedUser))
findUnverifiedUserByEmail conn email = do
  results <- try $ (query conn sql (Only email) :: IO [UnverifiedUser])
  case results of
    Left err@SqlError{}   -> return $ Left RepoError
    Right unverifiedUsers -> return $ Right $ maybeGet unverifiedUsers 0
  where
    sql = "select * from unverified_users where email = ?"

findUnverifiedUserByToken :: Connection -> UUID -> IO (Either AvtorError (Maybe UnverifiedUser))
findUnverifiedUserByToken connection uuid = do
  sqlResults <- try $ (query connection sql (Only uuid) :: IO [UnverifiedUser])
  case sqlResults of
    Left  err@SqlError{}  -> return $ Left RepoError
    Right unverifiedUsers -> return $ Right $ maybeGet unverifiedUsers 0
  where
    sql = "select * from unverified_users where token = ?"



maybeGet :: [a] -> Int -> Maybe a
maybeGet items index =
  if (Data.List.length items) >= index + 1
    then Just $ items !! index
    else Nothing

defaultUser :: IO User
defaultUser = do
  uuid1 <- nextRandom
  uuid2 <- nextRandom
  return User { 
    userId    = UserId uuid1, 
    userEmail = "", 
    userPass  = "", 
    userAccountId = AccountId uuid2
  }