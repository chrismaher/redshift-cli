{-# LANGUAGE OverloadedStrings #-}

module Redshift
    ( queryRunner
    , selectSchemas
    , selectTables
    ) where

import           Control.Monad.Reader
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import qualified Database.PostgreSQL.Simple as SQL
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.FromRow

import Config

type Name = String
type Prefix = String

type Connection = ReaderT SQL.Connection

data Row = SchemaRow (T.Text, Int)
         | TableRow (T.Text, T.Text, T.Text)
         deriving (Show)

queryRunner :: IO (Connection m a -> m a)
queryRunner = filterPGPass >>= \conn -> fmap (flip runReaderT) $ SQL.connect conn

unpack :: SQL.Only T.Text -> String
unpack (SQL.Only col) = T.unpack col

toIdentifier :: String -> Action
toIdentifier = EscapeIdentifier . encodeUtf8 . T.pack

selectSchemas :: Prefix -> Connection IO [Row]
selectSchemas prefix = do
    db <- ask
    liftIO $ (fmap . fmap) SchemaRow $ SQL.query db q [p]
    where
        p = prefix ++ "%"
        q = "SELECT nspname AS name, nspowner AS owner \
             \ FROM pg_namespace \
             \ WHERE nspowner <> 1 AND nspname LIKE ? \
             \ ORDER BY nspname"

selectTables :: Name -> Prefix -> Connection IO [Row]
selectTables schema prefix = do
    db <- ask
    liftIO $ (fmap . fmap) TableRow $ SQL.query db q [schema]
    where
        q = "SELECT schemaname, tablename, tableowner \
             \ FROM pg_tables \
             \ WHERE schemaname = ?"
