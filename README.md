# Symbolic Prover API
REST API server for zkFold Symbolic proof generation. Leverages the [Symbolic](https://github.com/zkFold/symbolic/tree/main) framework.


# Run server
Before running, you need to make sure you have installed `sqlite3`.

For run simple prover server
```bash
cabal run zkfold-prover-api -- --port 8083
```

# Example query
```
curl -X POST -H "Content-Type: application/json" -d @examples/simple-witness.json localhost:8083/v0/prove-unencrypted
```

# Tests
On the way!
