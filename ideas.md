- aggressive autocomplete
- better scripting syntax
	- scripts should have a lightweight concept of modules, to be able to import and reuse script logic
- first class argument parsing
- who even needs tmux
- implement a TUI scrollbar for the output buffer that can be dragged with a mouse
- add action boxes, like one for each command's output to save that output to a file, and one for the whole buffer
	- when clicked, the action would expand into a dropdown menu, with actions like save, which would then trigger a text input for a file name
	- have a runtime stats box for each executed command
- input line can expand to be multiline (up to 3 or 5) and scrollable using shift-enter or incomplete ASTs.
- pipe objects, not text
- non-sucky built-ins
- did you say Windows support!?
- Linux too, obv.
- cd directly into compressed archives
- cat images to terminal (not exactly new, but would be awesome)
- dynamically refreshing commands
	- if you `ls` a directory, updates to that directory should rewrite the previous output of `ls`. maybe this should require a flag.
- have a TUI file browser built-in

```
$files = ls
$num = len $files
if $num > 3 {
	echo "awesome folder here"
	$num += 5
	echo "it would be more awesome with $num files!"
} else {
	echo "lame folder here"
}
```

or maybe commands should be treated as function calls? like `echo("does this work?")`