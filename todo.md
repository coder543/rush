- for loops are currently broken because iterators are not a thing yet
- add more builtins, especially related to getting file properties and reading/writing files
- need to be able to pipe commands into other commands, probably
- support background tasks
- be able to send signals to child processes
- add a break statement
- provide some way to catch errors/panics
- replace character numbers with line and column
	- since the buffer for the script exists for the whole time the AST does anyways, maybe we don't need "raw"?
- test that the AST parser doesn't choke on the lack of a semicolon in most places
- untested, but I'm pretty confident that there is a hole in the variable scoping logic where the current scope is transferred into function calls
- the interactive shell needs to be rewritten almost entirely
	- each script (at a minimum) will run on a separate thread, transmitting output back through a channel
	- this means that all commands will be asynchronous
	- commands can run until they choose to exit or the user kills them
	- since output is directed over unique channels for each process, updated output from a particular process will be able to be output to the correct location
	- the interactive shell...
		- currently does not understand wide characters like tabs, which means text is allowed to overflow the right-hand side and cause a crash
		- does not support scrolling or even showing only the latest output, it just shows the earliest output
		- needs to support inline input fields, buttons, scrollbars, and other exciting things
