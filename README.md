# Symbolic Prover API
REST API server for zkFold Symbolic proof generation. Leverages the [Symbolic](https://github.com/zkFold/symbolic/tree/main) framework.


# Before start
Before running, you need to make sure you have installed `sqlite3`.
```bash
sudo apt-get install sqlite3 libsqlite3-dev
```

# Start server
You can specify server parameters in config file
```yaml
serverPort: 8083
dbFile: sqlite.db
nWorkers: 2
proverMode: encrypted
proofLifetime: 30
keysLifetime: 86400
```

Also you can use cli arguments
```bash
cabal run zkfold-prover-api -- 
    \ --port 8083 
    \ --db-file sqlite.db
    \ --mode encrypted
    \ --n-workers 2
    \ --proof-lifetime 30
    \ --keys-lifetime 86400
    \ --config config.yaml
```
Command line arguments take precedence over configuration file arguments. If no values ​​are specified, default values ​​are used.

For run simple prover server
```bash
cabal run zkfold-prover-api
```

# Example query
You can test the server by sending the following request
```
curl -X GET -H "Content-Type: application/json" localhost:8083/v0/stats
```
You should see something akin to
```json
{
    "total_valid_proofs":0,
    "longest_queue_size":0,
    "average_proof_time_seconds":0.0
}
```

# Tests
On the way!
