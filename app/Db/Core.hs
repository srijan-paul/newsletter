{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Db.Core
  ( openDB,
    Db,
    OpenDb,
    Subscriber (..),
    insertSubscriber,
    fetchActiveSubscribers,
    unsubscribeUser,
  )
where

import Data.Foldable (foldrM)
import qualified Data.Text as T
import Database.SQLite.Simple
  ( Connection,
    Only (Only),
    Statement,
    close,
    closeStatement,
    execute,
    execute_,
    nextRow,
    open,
    openStatement,
    query,
    query_,
    withBind,
  )
import Database.SQLite.Simple.FromRow (FromRow (..), field)
import Database.SQLite.Simple.ToRow (ToRow (..))
import Relude
import System.Directory
import System.Environment.XDG.BaseDir (getUserDataDir)
import System.FilePath

-- a type level enum using "DataKinds"
data ConnectionState = Open | Closed

newtype Db (state :: ConnectionState) = Db Connection

type OpenDb = Db Open

-- | Open a connection to the local SQLite database.
openDB :: IO (Db Open)
openDB = do
  dbDir <- getUserDataDir "subscribers"
  let dbPath = dbDir </> "mails.db"

  dbExists <- doesFileExist dbPath
  unlessM (doesFileExist dbPath)
    $ createDirectoryIfMissing True dbDir
  conn <- open dbPath
  initDB conn

closeDB :: Db Open -> IO (Db Closed)
closeDB (Db conn) = do
  close conn
  return $ Db conn

-- | The Subscriber model.
data Subscriber = Subscriber
  { subscriberEmail :: T.Text,
    -- | Unique ID identifying a subscriber.
    -- This is used in the 'unsubscribe' links sent to the subscribers.
    -- If we just used the email ID to unsub, then a bad actor could remove somebody else from the subscribers list.
    subscriberId :: T.Text,
    -- TODO: use LocalTime with a ToField and FromField instance
    subscriberDate :: T.Text,
    -- | When a subscriber unsubs, we flip this to `False` instead of deleting the row.
    -- Rule #1 of spying on unsuspecting users – never delete their data! (JK)
    subscriberIsSubbed :: Bool
  }
  deriving (Generic, Show, Eq)

instance FromRow Subscriber

instance ToRow Subscriber

collectRows :: forall r. (FromRow r) => Statement -> IO [r]
collectRows stmt = go []
  where
    go :: [r] -> IO [r]
    go acc = do
      r <- nextRow @r stmt
      case r of
        Just row -> go (row : acc)
        Nothing -> return acc

fetchActiveSubscribers :: Db Open -> Subscriber -> IO [Subscriber]
fetchActiveSubscribers (Db conn) subscriber =
  query_ conn "SELECT * FROM subscribers WHERE is_subbed = 1"

insertSubscriber :: Db Open -> Subscriber -> IO ()
insertSubscriber (Db conn) subscriber = do
  execute
    conn
    "INSERT INTO subscribers \
    \ (email, id, date, is_subbed) VALUES (?, ?, ?, ?) \
    \ ON CONFLICT DO UPDATE SET is_subbed = 1"
    subscriber
  return ()

findSubscriberById :: Db Open -> T.Text -> IO (Maybe Subscriber)
findSubscriberById (Db conn) sid = do
  rows <- query conn "SELECT * FROM subscribers WHERE id = ?" (Only sid)
  return $ listToMaybe rows

unsubscribeUser :: Db Open -> T.Text -> IO ()
unsubscribeUser (Db conn) sid =
  execute conn "UPDATE subscribers SET is_subbed = 0 WHERE id = ?" (Only sid)

initDB :: Connection -> IO (Db Open)
initDB conn = do
  -- create the subscribers table.
  execute_ conn "CREATE TABLE IF NOT EXISTS subscribers (email TEXT PRIMARY KEY, id TEXT, date TEXT, is_subbed BOOLEAN)"
  return $ Db conn
