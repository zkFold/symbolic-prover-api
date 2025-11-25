# Symbolic Prover API
REST API server for zkFold Symbolic proof generation. Leverages the [Symbolic](https://github.com/zkFold/symbolic/tree/main) framework.


# Prerequisites
Before running, you need to make sure you have installed `sqlite3`.
```bash
sudo apt-get install sqlite3 libsqlite3-dev
```

# Running the server
You must provide a configuration file path when starting the server. Pass it via `--config PATH` (or the short form `-c PATH`). The file can look like:
```yaml
serverPort: 8083
dbFile: sqlite.db
nWorkers: 2
proofLifetime: 30
keysLifetime: 86400
```

To start the server, run:
```bash
cabal run symbolic-prover-api -- -c config.yaml
```

# Docs
You can view the documentation after starting the server using the /docs endpoint.
Example: http://localhost:8083/docs/

# Tests
On the way!
