module ZkFold.Prover.API.Types.Config where

import Control.Monad (unless)
import Data.Aeson
import Data.Yaml (decodeFileThrow)
import GHC.Generics
import Options.Applicative

import System.Directory (doesFileExist)

data ServerConfig = ServerConfig
    { serverPort :: Int
    -- ^ Server port (default: 8083)
    , dbFile :: String
    -- ^ Path to SQLite database file (default: ./sqlite.db)
    , nWorkers :: Int
    -- ^ Number of worker threads (default: 2)
    , proofLifetime :: Int
    -- ^ Proof lifetime in days (default: 30 days)
    , keysLifetime :: Int
    -- ^ Key lifetime in seconds (default: 24 hours)
    }
    deriving (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

configPathParser :: Parser FilePath
configPathParser =
    strOption
        ( long "config"
            <> short 'c'
            <> help "Path to server configuration yaml file"
            <> metavar "PATH"
        )

parseConfig :: IO ServerConfig
parseConfig = do
    configPath <- execParser opts
    fileExist <- doesFileExist configPath
    unless fileExist $ fail $ "Config file not found: " <> configPath
    decodeFileThrow configPath
  where
    opts =
        info
            (configPathParser <**> helper)
            ( fullDesc
                <> progDesc "Smart Wallet prover"
                <> header "zkFold's Smart Wallet prover server"
            )
