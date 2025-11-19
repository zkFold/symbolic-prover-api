APP_NAME=symbolic-prover-api
DOCKER_IMAGE=$(APP_NAME):latest
PORT ?= 8080

all: build

build:
	cabal update
	cabal build

run: build
	PORT=$(PORT) cabal run symbolic-prover-api

# docker-build:
# 	docker build -t $(DOCKER_IMAGE) .

# docker-run: docker-build
# 	docker run -e PORT=$(PORT) -p $(PORT):$(PORT) $(DOCKER_IMAGE)

clean:
	cabal clean

.PHONY: all build run docker-build docker-run clean
