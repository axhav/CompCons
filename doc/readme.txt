To run compiler:
- Run make in ./src directory
- Now a runable file is located in the installed directory.
- ./jlc fileName.jl
- Has now generated a executable file named a.out

NOTE:
You can only run jlc when you are located in the main installed directory. 
Because or liking to lib directory doesn't work otherwise.  

Javalette language specifications can be found in ./src/DocJavalette.txt after
running the command make.

Shift/reduce conflicts: 1
Dangling else
This is a unavoidable conflict if you implement both if and if-else statements. 
When the parser finds an if token then it has two possible rules to follow. This
results in a conflict.

