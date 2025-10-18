# Makefile for the m28 Lisp interpreter project

# Variables
BINARY_NAME=m28
BINARY_DIR=bin
SRC_DIR=./
PKG_DIRS=$(shell go list ./... | grep -v /vendor/)

# Default target
all: build

# Build the project
build:
	@echo "Building $(BINARY_NAME)..."
	@go build -o $(BINARY_DIR)/$(BINARY_NAME) $(SRC_DIR)/main.go

# Run tests
test:
	@echo "Running tests..."
	@go test $(PKG_DIRS)

# Run m28 language tests
m28-test:
	@echo "Running m28 language tests..."
	@./test.sh

# Quick test (essential tests only)
test-quick:
	@./test.sh --quick

# Full test suite (includes stress tests)
test-full:
	@./test.sh --full

# CPython conformance tests
test-cpython:
	@./test.sh --cpython

# Clean build artifacts
clean:
	@echo "Cleaning up..."
	@rm -rf $(BINARY_DIR)/$(BINARY_NAME)

# Install dependencies
deps:
	@echo "Installing dependencies..."
	@go mod tidy
	@go get -v ./...

# Run the REPL
run: build
	@echo "Starting the REPL..."
	@./$(BINARY_DIR)/$(BINARY_NAME)

# Help
help:
	@echo "Makefile commands:"
	@echo "  make build        - Compile the project."
	@echo "  make test         - Run Go tests."
	@echo "  make m28-test     - Run m28 language tests."
	@echo "  make test-quick   - Run essential tests only."
	@echo "  make test-full    - Run full test suite including stress tests."
	@echo "  make test-cpython - Run CPython conformance tests."
	@echo "  make clean        - Clean build artifacts."
	@echo "  make deps         - Install and tidy up dependencies."
	@echo "  make run          - Start the REPL."
	@echo "  make fmt          - Format Go code using gofmt."
	@echo "  make all          - Default, build the project."
	@echo "  make help         - Display this help."

# Format the code
fmt:
	@echo "Formatting Go code..."
	@gofmt -w -s $(shell find . -name "*.go" -not -path "./vendor/*")

# Special targets
.PHONY: all build test m28-test clean deps run help fmt

