# Symbolic Prover API
REST API server for zkFold Symbolic proof generation. Leverages the [Symbolic](https://github.com/zkFold/symbolic/tree/main) framework.

# Build
## Source
The package compiles with GHC 9.12.1 and Cabal 3.14.1.1.
```
make build
```
<!-- Docker currently does not support ghc version 9.12.1 -->
<!-- ## Docker
```
make docker-build
``` -->
# Run
## Host
```
make run
```
<!-- ## Docker
```
make docker-run
``` -->

# Example query
```
curl -X POST -H "Content-Type: application/json" -d @examples/simple-witness.json localhost:8080/prove
```

# Tests
On the way!
