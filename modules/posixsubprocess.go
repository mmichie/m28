package modules

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// Init_PosixsubprocessModule creates the _posixsubprocess module stub
// This is a C extension module that provides fork_exec for subprocess
func Init_PosixsubprocessModule() *core.DictValue {
	posixsubprocessModule := core.NewDict()

	// fork_exec(args, executable_list, close_fds, ...) - stub
	// This is used by subprocess.Popen to create child processes
	// For now, just return a stub that raises NotImplementedError when called
	posixsubprocessModule.SetWithKey("fork_exec", core.StringValue("fork_exec"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return nil, fmt.Errorf("NotImplementedError: _posixsubprocess.fork_exec not implemented in M28")
	}))

	return posixsubprocessModule
}
