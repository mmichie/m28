package embed

import (
	"os/exec"
)

// DefaultShellExecutor executes a shell command and returns its output
func DefaultShellExecutor(command string) (string, error) {
	cmd := exec.Command("sh", "-c", command)
	output, err := cmd.CombinedOutput()
	return string(output), err
}
