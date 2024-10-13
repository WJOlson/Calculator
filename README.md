#Calculator

A calculator console application. 
The windows x64 version is found in "win x64".

It features:
	1. Expressions.
	2. Variables.
	3. Functions (without recursion).

Commands:
Anything within square brackets is either a token or a grammatical rule.

	del [identifier]: deletes a variable.
	let [identifier] '=' [number]: creates a global variable.
	fnt [identifier] '(' [parameters] ')' '=' [expression]: defines a new function.

Building with CMake:
Requires cmake to build:
	build command: cmake . -B ./build