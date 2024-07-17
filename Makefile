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
	@echo "  make build  - Compile the project."
	@echo "  make test   - Run tests."
	@echo "  make clean  - Clean build artifacts."
	@echo "  make deps   - Install and tidy up dependencies."
	@echo "  make run    - Start the REPL."
	@echo "  make all    - Default, build the project."
	@echo "  make help   - Display this help."

# Special targets
.PHONY: all build test clean deps run help

