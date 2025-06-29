package main

import (
	"fmt"
	"log"
	"os"

	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/embed"
)

// Example: Using M28 as a configuration language
func main() {
	engine := embed.NewM28Engine()

	// Define some helper functions for configuration
	var config = make(map[string]interface{})

	engine.DefineFunction("config-set", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, fmt.Errorf("config-set expects 2 arguments")
		}

		key, ok := args[0].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("config key must be string")
		}

		// Store the raw value
		config[string(key)] = args[1]
		return core.Nil, nil
	})

	engine.DefineFunction("env", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 || len(args) > 2 {
			return nil, fmt.Errorf("env expects 1 or 2 arguments")
		}

		varName, ok := args[0].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("env variable name must be string")
		}

		value := os.Getenv(string(varName))
		if value == "" && len(args) == 2 {
			// Use default value
			return args[1], nil
		}

		return core.StringValue(value), nil
	})

	// Example configuration file content
	configScript := `
# Application configuration
(config-set "app.name" "MyApp")
(config-set "app.version" "1.0.0")

# Database configuration with environment variables
(config-set "db.host" (env "DB_HOST" "localhost"))
(config-set "db.port" (env "DB_PORT" "5432"))
(config-set "db.name" (env "DB_NAME" "myapp"))

# Feature flags
(config-set "features.auth" true)
(config-set "features.api_v2" false)

# Computed configuration
(config-set "db.url" 
  (+ "postgresql://" 
     (env "DB_HOST" "localhost") ":" 
     (env "DB_PORT" "5432") "/" 
     (env "DB_NAME" "myapp")))

# Server configuration
(config-set "server.port" 8080)
(config-set "server.workers" (* 2 4))
`

	// Load configuration
	_, err := engine.Evaluate(configScript)
	if err != nil {
		log.Fatal("Failed to load configuration:", err)
	}

	// Display loaded configuration
	fmt.Println("Loaded Configuration:")
	fmt.Println("====================")
	for key, value := range config {
		fmt.Printf("%s = %v\n", key, value)
	}

	// You can also load from a file
	// err = engine.ExecuteFile("config.m28")
}
