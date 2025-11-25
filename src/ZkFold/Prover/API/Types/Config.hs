module ZkFold.Prover.API.Types.Config where

import Data.Yaml (decodeFileThrow)
import GHC.Generics
import Options.Applicative

import System.Directory (doesFileExist)

import Data.Aeson

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
    option
        str
        ( long "config"
            <> help "Path to server configuration yaml file"
            <> showDefault
            <> value ""
            <> metavar "PATH"
        )

defaultServerConfig :: ServerConfig
defaultServerConfig =
    ServerConfig
        { serverPort = 8083
        , dbFile = "./sqlite.db"
        , nWorkers = 2
        , proofLifetime = 30
        , keysLifetime = 86400
        }

cliParser :: ServerConfig -> Parser (FilePath, ServerConfig)
cliParser ServerConfig{..} = do
    liftA2 (,) configPathParser $
        ServerConfig
            <$> portParser serverPort
            <*> dbFileParser dbFile
            <*> nWorkersParser nWorkers
            <*> proofLifetimeParser proofLifetime
            <*> keysLifetimeParser keysLifetime
  where
    dbFileParser :: FilePath -> Parser FilePath
    dbFileParser d =
        option
            str
            ( long "db-file"
                <> help "Path to SQLite database file"
                <> showDefault
                <> value d
                <> metavar "PATH"
            )

    portParser :: Int -> Parser Int
    portParser d =
        option
            auto
            ( long "port"
                <> short 'p'
                <> help "Server port"
                <> showDefault
                <> value d
                <> metavar "PORT"
            )

    nWorkersParser :: Int -> Parser Int
    nWorkersParser d =
        option
            auto
            ( long "n-workers"
                <> short 'w'
                <> help "Number of worker threads"
                <> showDefault
                <> value d
                <> metavar "N"
            )

    proofLifetimeParser :: Int -> Parser Int
    proofLifetimeParser d =
        option
            auto
            ( long "proof-lifetime"
                <> help "Proof lifetime in days"
                <> showDefault
                <> value d
                <> metavar "DAYS"
            )

    keysLifetimeParser :: Int -> Parser Int
    keysLifetimeParser d =
        option
            auto
            ( long "keys-lifetime"
                <> help "Key lifetime in seconds"
                <> showDefault
                <> value d
                <> metavar "SECONDS"
            )

parseConfig :: IO ServerConfig
parseConfig = do
    (configPath, cliConfig) <- execParser $ opts defaultServerConfig
    fileExist <- doesFileExist configPath
    if fileExist
        then do
            fileConfig <- decodeFileThrow configPath
            snd <$> execParser (opts fileConfig)
        else do
            pure cliConfig
  where
    opts defaultConfig =
        info
            (cliParser defaultConfig <**> helper)
            ( fullDesc
                <> progDesc "Smart Wallet prover"
                <> header "zkFold's Smart Wallet prover server"
            )
