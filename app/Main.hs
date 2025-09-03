{-# LANGUAGE OverloadedStrings #-}

import Options.Applicative
import ZkFold.Prover.API.Server
import ZkFold.Prover.API.Database (taskManager)
import Control.Concurrent

portParser ∷ Parser Int
portParser = 
  option
      auto
      ( long "port"
          <> help "Port to listen for proof requests"
          <> showDefault
          <> value 8083
          <> metavar "INT"
      )

main ∷ IO ()
main = do
  port ← execParser opts
  let
    dbHost = "localhost" 
    dbName = "postgres"
    dbUser = "postgres"
    dbPassword = ""
    nWorkers = 3

  let serverConfig = ServerConfig {..}
  print @String ("Started with config: " <> show serverConfig)
  runServer serverConfig
 where
  opts =
    info
      (portParser <**> helper)
      ( fullDesc
          -- <> progDesc "Smart Wallet prover"
          -- <> header "zkFold's Smart Wallet prover server"
      )
