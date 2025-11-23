module ZkFold.Prover.API.Types.Config where

import Data.Yaml (decodeFileThrow)
import GHC.Generics
import Options.Applicative
import ZkFold.Prover.API.Types.Ctx (ProverMode (..))

import System.Directory (doesFileExist)

import Data.Aeson

data ServerConfig = ServerConfig
    { serverPort :: Int
    -- ^ Server port (default: 8083)
    , dbFile :: String
    -- ^ Path to SQLite database file (default: ./sqlite.db)
    , nWorkers :: Int
    -- ^ Number of worker threads (default: 2)
    , proverMode :: ProverMode
    -- ^ Prover mode: Encrypted or Plain (default: Encrypted)
    , proofLifetime :: Int
    -- ^ Proof lifetime in days (default: 30 days)
    , keysLifetime :: Int
    -- ^ Key lifetime in seconds (default: 24 hours)
    , delegationServers :: [String]
    -- ^ Servers which can be used to delegate prove
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
        , proverMode = Encrypted
        , proofLifetime = 30
        , keysLifetime = 86400
        , delegationServers = []
        }

cliParser :: ServerConfig -> Parser (FilePath, ServerConfig)
cliParser ServerConfig{..} = do
    liftA2 (,) configPathParser $
        ServerConfig
            <$> portParser serverPort
            <*> dbFileParser dbFile
            <*> nWorkersParser nWorkers
            <*> modeParser proverMode
            <*> proofLifetimeParser proofLifetime
            <*> keysLifetimeParser keysLifetime
            <*> delegationServersParser delegationServers
  where
    modeReader :: ReadM ProverMode
    modeReader = do
        arg <- str
        case arg of
            "encrypted" -> return Encrypted
            "plain" -> return Plain
            _ -> readerError $ "Invalid value: " ++ arg ++ ". Allowed values: encrypted, unencrypted."

    modeParser :: ProverMode -> Parser ProverMode
    modeParser d =
        option
            modeReader
            ( long "mode"
                <> help "Encryption mode: encrypted or plain"
                <> short 'm'
                <> showDefault
                <> value d
                <> metavar "MODE"
            )

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

    delegationServersParser :: [String] -> Parser [String]
    delegationServersParser d =
        (\x -> if null x then d else x)
            <$> many
                ( strOption
                    ( long "server"
                        <> short 's'
                        <> metavar "URL"
                        <> help "URL for delegating prover"
                    )
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
