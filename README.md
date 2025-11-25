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

# Example query
Once the server is running you can send a test request to get the public keys exposed by the encrypted proving API:
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

Submit unencrypted witnesses via `POST /v0/prove` or encrypted payloads via `POST /v0/prove-encrypted`.

# Tests
On the way!
