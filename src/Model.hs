{-# LANGUAGE OverloadedStrings #-}

module Model where

import Database.Beam

import Calamity (Message, Snowflake (..), User)
import Database.Beam.Migrate
import Database.Beam.Migrate.Simple
import Database.Beam.Sqlite (Sqlite, runBeamSqliteDebug)
import Database.Beam.Sqlite.Migrate
import Database.SQLite.Simple
import Shower (printer)

import Db.Snowflake ()

data MessageT f = Message
  { _messageId :: Columnar f (Snowflake Message)
  , _messageAuthor :: Columnar f (Snowflake User)
  , _messageContent :: Columnar f Text
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

type MessageLog = MessageT Identity
deriving stock instance Show MessageLog
deriving stock instance Eq MessageLog

instance Table MessageT where
  data PrimaryKey MessageT f = MessageId (Columnar f (Snowflake Message))
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = MessageId . _messageId

newtype DevDenDB f = DevDenDB
  { _messages :: f (TableEntity MessageT)
  }
  deriving stock (Generic)
  deriving anyclass (Database be)

devdenDB :: CheckedDatabaseSettings Sqlite DevDenDB
devdenDB = defaultMigratableDbSettings @Sqlite

initDb :: IO ()
initDb = do
  conn <- open ":memory:"
  runBeamSqliteDebug putStrLn conn $ do
    autoMigrate migrationBackend devdenDB

    runInsert $
      insert (_messages $ unCheckDatabase devdenDB) $
        Database.Beam.insertValues
          [ Message (Snowflake 1) (Snowflake 2) "Hello, World!"
          , Message (Snowflake 3) (Snowflake 4) "pwdqfep"
          ]

    messages <- runSelectReturningList $ select $ all_ (_messages $ unCheckDatabase devdenDB)
    liftIO $ printer messages
