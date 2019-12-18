{-#LANGUAGE ScopedTypeVariables#-}

module CLI
    ( optParse
    , Command (..)
    ) where

import Options.Applicative

type Schema = String
type Prefix = String

data Command = ShowSchemas Prefix
             | ShowTables  Schema Prefix

optsParser :: ParserInfo Command
optsParser = info (helper <*> subParser) description
    where
        subParser = hsubparser (showSchemas <> showTables)
        description = fullDesc <> progDesc "Redshift CLI" <> header ""

schema :: Parser String
schema = strOption (long "schema" <> short 's' <> metavar "SCHEMA" <> help "Schema of the thing to list")

prefix :: Parser String
prefix = strOption (long "prefix" <> short 'p' <> metavar "PREFIX" <> help "Filter tables to those including the prefix")

createCommand :: String -> Parser a -> String -> Mod CommandFields a
createCommand name opts desc = command name (info opts (progDesc desc))

showSchemas :: Mod CommandFields Command
showSchemas = createCommand "show-schemas" showOptions "Lists schemas"
    where
        showOptions = ShowSchemas <$> prefix

showTables :: Mod CommandFields Command
showTables = createCommand "show-tables" showOptions "Lists tables"
    where
        showOptions = ShowTables <$> schema <*> prefix

optParse :: IO Command
optParse = execParser optsParser
