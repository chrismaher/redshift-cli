module Main where

import CLI
import Redshift

main :: IO ()
main = do
    runner <- queryRunner
    opts <- optParse
    let run f = runner f >>= mapM_ print
    case opts of
        ShowSchemas p -> run (showSchemas p)
        ShowTables s p -> run (showTables s p)
