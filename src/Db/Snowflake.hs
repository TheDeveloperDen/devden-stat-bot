{-# OPTIONS_GHC -Wno-orphans #-}

module Db.Snowflake where

import Calamity
import Database.Beam.Backend
import Database.Beam.Backend.SQL.AST
import Database.Beam.Migrate
import Database.Beam.Sqlite
import Database.Beam.Sqlite.Syntax

instance HasSqlValueSyntax Value (Snowflake f) where
  sqlValueSyntax = Value . fromSnowflake

instance HasSqlValueSyntax SqliteValueSyntax (Snowflake f) where
  sqlValueSyntax = sqlValueSyntax . fromSnowflake

instance HasDefaultSqlDataType Sqlite (Snowflake f) where
  defaultSqlDataType _ = defaultSqlDataType (Proxy @Word64)

instance FromBackendRow Sqlite (Snowflake f) where
  fromBackendRow = Snowflake <$> fromBackendRow
