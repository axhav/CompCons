How to run:
    To run compiler:
    - Run make in ./src directory
    - Now a runable file is located in the installed directory.
    - ./jlc fileName.jl
    - The compiler has now generated a executable file named a.out

    To compile for x86:
    - Same as for llvm.
    - Need the argument -x86. Note: -x86 must be the second argument.
    - ./jlc fileName.jl -x86

NOTES:
    You can only run jlc when you are located in the main installed directory. 
    Because or liking to lib directory doesn't work otherwise.  
    
    We have added a test for multi-array extension that test:
        - Three demension array can be created.
        - Test that function can return multi-arrays and that passing a 
          multi-arrays to a function works.
          
Extension:
    - Single array with foreach.
    - Multi arrays.
    - x86 code generation.

Other information:
    Javalette language specifications can be found in ./src/DocJavalette.txt after
    running the command make.

    Shift/reduce conflicts: 2
    
    Dangling else
    This is a unavoidable conflict if you implement both if and if-else statements. 
    When the parser finds an if token then it has two possible rules to follow. This
    results in a conflict.
    
    Bracket problem
    This shift reduce is caused by indexing of multiarrays. Where can not 
    determent if there will be more brackets or not.
