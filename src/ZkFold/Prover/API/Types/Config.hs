module ZkFold.Prover.API.Types.Config where

import Options.Applicative
import ZkFold.Prover.API.Types.Ctx (ProverMode (..))
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import System.Directory (doesFileExist)
import Data.Yaml (decodeFileThrow)

data ServerConfig = ServerConfig
    { serverPort :: Int
    , dbFile :: String
    , nWorkers :: Int
    , proverMode :: ProverMode
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
            <> value "config.yaml"
            <> metavar "PATH"
        )

defaultServerConfig :: ServerConfig
defaultServerConfig =
    ServerConfig
        { serverPort = 8083
        , dbFile = "./sqlite.db"
        , nWorkers = 4
        , proverMode = Encrypted
        }

cliParser :: ServerConfig -> Parser (FilePath, ServerConfig)
cliParser ServerConfig{..} = do
    liftA2 (,) configPathParser $
        ServerConfig
            <$> portParser serverPort
            <*> dbFileParser dbFile
            <*> nWorkersParser nWorkers
            <*> modeParser proverMode
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
                <> help "Encryption mode: encrypted or decrypted"
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