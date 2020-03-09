{-# LANGUAGE OverloadedStrings #-}

module Config (filterPGPass) where

import qualified Control.Exception as E
import           Control.Monad.Except
import qualified Data.ByteString as B
import           Data.Char (digitToInt)
import           Data.Functor.Identity (Identity)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.Word (Word16)
import qualified Data.Yaml as Y
import           Database.PostgreSQL.Simple
import           System.Directory (getHomeDirectory)
import           System.FilePath (FilePath, joinPath)
import           Text.ParserCombinators.Parsec
import           Text.Parsec.Prim (ParsecT)

data ConfigError = ConfigError String
                 | IOError E.IOException
                 deriving (Show)

instance E.Exception ConfigError

instance Y.FromJSON ConnectInfo where
    parseJSON (Y.Object v) = do
        dbname_ <- ((v Y..: "default") >>= (Y..: "dbname"))
        user_ <- ((v Y..: "default") >>= (Y..: "user"))
        return $ defaultConnectInfo { connectDatabase = dbname_, connectUser = user_ }

connectInfo :: String -> Word16 -> String -> String -> String -> ConnectInfo
connectInfo host port dbname user pword =
    ConnectInfo { connectHost = host
                , connectPort = port
                , connectUser = user
                , connectPassword = pword
                , connectDatabase = dbname
                }

packStr'' :: String -> B.ByteString
packStr'' = encodeUtf8 . T.pack

word16 :: (Integral a) => a -> Word16
word16 = fromIntegral

fullPath :: FilePath -> IO FilePath
fullPath path = getHomeDirectory >>= \dir -> return $ joinPath [dir, path]

withFailure :: (MonadIO m, MonadError e m) => IO (Either e b) -> m b
withFailure f = liftIO f >>= either throwError return

readFile' :: FilePath -> ExceptT ConfigError IO String
readFile' = withFailure . try' . readFile
    where
        try' a = E.catch (Right `liftM` a) (return . Left)

pgParser :: ParsecT String u Identity ConnectInfo
pgParser = connectInfo <$> term <*> (word16 <$> port') <*> term <*> term <*> value
    where
        value = many (noneOf ":\n")
        term = value <* char ':'
        port' = foldl (\a i -> a * 10 + digitToInt i) 0 <$> many1 digit <* char ':'

parsePGPass :: String -> Either ConfigError [ConnectInfo]
parsePGPass str =
    case parse pgPassFile "" str of
        Left _ -> Left $ ConfigError "Could not parse credentials from .pgpass"
        Right x -> Right x
    where
        pgPassFile = endBy pgParser (char '\n')

parseConfig :: String -> ExceptT ConfigError IO ConnectInfo
parseConfig str = withFailure $ do
    return $ case Y.decodeEither' (packStr'' str) of
            Left _ -> Left $ ConfigError "Could not parse .redshift/config.yaml"
            Right x -> Right x

filterPass :: ConnectInfo -> [ConnectInfo] -> Either ConfigError ConnectInfo
filterPass pass passes =
    let user'    = ((==) $ connectUser pass) . connectUser
        dbname'  = ((==) $ connectDatabase pass) . connectDatabase
        filtered = filter (liftM2 (&&) user' dbname') passes
    in case filtered of
        (pass:[]) -> Right pass
        _         -> Left $ ConfigError "failed to match user & dbname from config.yaml to credentials in .pgpass"

filterPGPass :: IO ConnectInfo
filterPGPass = either E.throw return <=< runExceptT $ do
    config <- (liftIO (fullPath ".redshift/config.yaml") >>= readFile')
    pgpass <- (liftIO (fullPath ".pgpass") >>= readFile')
    pass <- parseConfig config
    pg <- either throwError return $ parsePGPass pgpass
    either throwError return $ filterPass pass pg
