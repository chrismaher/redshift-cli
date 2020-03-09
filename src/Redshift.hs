module Redshift
    ( queryRunner
    , showSchemas
    , showTables
    ) where

import           Control.Monad.Reader
import qualified Data.ByteString as B
import qualified Database.PostgreSQL.Simple as SQL
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.Types (Query (..))
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           System.FilePath (joinPath)

import Config
import Paths_redshift_cli (getDataFileName)

type Object = String
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

readQuery :: FilePath -> IO Query
readQuery fname = getDataFileName path >>= readQuery'
    where
        path = joinPath ["data/templates", fname]
        readQuery' = fmap Query . B.readFile

showSchemas :: Prefix -> Connection IO [Row]
showSchemas p = do
    db <- ask
    q <- liftIO $ readQuery "showSchemas.sql"
    liftIO $ (fmap . fmap) SchemaRow $ SQL.query db q [p ++ "%"]

showTables :: Object -> Prefix -> Connection IO [Row]
showTables schema prefix = do
    db <- ask
    q <- liftIO $ readQuery "showTables.sql"
    liftIO $ (fmap . fmap) TableRow $ SQL.query db q [schema]
