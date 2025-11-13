module ZkFold.Prover.API.Types.Config where

import Options.Applicative
import ZkFold.Prover.API.Types.Ctx (EncryptionMode (..))

data ServerConfig = ServerConfig
    { serverPort :: Int
    , dbFile :: String
    , nWorkers :: Int
    , contractId :: Int
    , encryptionMode :: EncryptionMode
    , proofLifetimeDays :: Int
    , keysLifetimeSeconds :: Int
    }
    deriving (Eq, Show)

defaultServerConfig :: ServerConfig
defaultServerConfig =
    ServerConfig
        { serverPort = 8083
        , dbFile = "./sqlite.db"
        , nWorkers = 4
        , contractId = 1
        , encryptionMode = EncryptedMode
        , proofLifetimeDays = 30
        , keysLifetimeSeconds = 86400
        }

cliParser :: ServerConfig -> Parser ServerConfig
cliParser ServerConfig{..} =
    ServerConfig
        <$> portParser serverPort
        <*> dbFileParser dbFile
        <*> nWorkersParser nWorkers
        <*> contractIdParser contractId
        <*> modeParser encryptionMode
        <*> proofLifetimeParser proofLifetimeDays
        <*> keysLifetimeParser keysLifetimeSeconds
  where
    modeReader :: ReadM EncryptionMode
    modeReader = do
        arg <- str
        case arg of
            "encrypted" -> return EncryptedMode
            "unencrypted" -> return UnencryptedMode
            _ -> readerError $ "Invalid value: " ++ arg ++ ". Allowed values: encrypted, unencrypted."

    modeParser :: EncryptionMode -> Parser EncryptionMode
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

    contractIdParser :: Int -> Parser Int
    contractIdParser d =
        option
            auto
            ( long "contract-id"
                <> short 'c'
                <> help "Contract ID"
                <> showDefault
                <> value d
                <> metavar "ID"
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