# Symbolic Prover API
REST API server for zkFold Symbolic proof generation. Leverages the [Symbolic](https://github.com/zkFold/symbolic/tree/main) framework.


# Before start
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

# Docs
You can view the documentation after starting the server using the /docs endpoint.
Example: http://localhost:8083/docs/

# Example query
Once the server is running in encrypted mode, you can send a test request to get keys:
```bash
curl -X GET localhost:8083/v0/keys
```
Example output:
```json
[
    {
        "id":"55ffae1d-cc75-4d70-9e1e-7eadbffa6a74",
        "public": {
            "public_e":"65537",
            "public_n":"0",
            "public_size":"2048"
        }
    },
    {
        "id":"48f085ea-9b6c-4839-8192-e60516be602c",
        "public": {
            "public_e":"65537",
            "public_n":"0",
            "public_size":"2048"
        }
    }
]
```

# Tests
On the way!
