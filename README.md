# Cascal-Interpreter
This is an interpreter for a custom programming language Cascal written in
Haskell.

## Prerequisites
In order to run the interpreter you need:
- ghci 7.10.3(Download at: https://www.haskell.org/ghc/)
- bnfc, Alex, Happy(Download at: http://bnfc.digitalgrammars.com/)

## Starting interpreter
To start the Interpreter type:
```
cd src/
make
./Interpreter program.cascal
```

or if you want to read the program from standard input
```
./Interpreter
```

## Files

* src/
  - Cascal.cf - grammar for the language in ebnf format
  - Validator.hs - static type checker of the program
  - Interpreter.hs - the actual interpreter executing code of the program
  - Makefile - builds the interpreter
  - Main.hs - reads the program description and starts type checker and interpreter

   
## Language specification:
The language contains among other things, the following:
- basic statements(assignment, if, while, etc.)
- functions with recursion
- 4types:int, bool, string, functional type
- runtime errors handling
- static typing of variables
- nested functions with static typing
- anonymous functions
- functions as parameters or returned as the result(closures)
- functions and variable redeclaration within the same block is supported

The language I am going to implement is a simplified C language with
elements from Pascal. Let's call it Cascal. 


To initialize a variable in Cascal:
```
    bool x = true;
```

The ";" sign is used after every instruction. This code will parse correctly:
```
    bool x;
    x = true;
    x = x || !x;
```
while this won't:
```
    bool x;
    x = true;
    x = x || !x
```

Cascal allows for creating functions that return values. Those functions
can take any number of parameters. Cascal follows "single exit point"
convention. This means that only one return statement is allowed and it
has to be placed at the end of the function. Function body is defined within
"begin" "end" terms.

Also Cascal allows for redeclaration of variables and functions within
the same scope.
```
    int x;
    x = 5;
    string x;
    x = "asd";
```

```
    int sum(int x, int y) begin
        return x+y;
    end;
    
    int z = sum(1,2);
    
    int sum(int x, int y, int z) begin
        return x+y+z;
    end;
    int z = sum(1,2,3);
```


To see more examples of how Cascal work check out the examples in folders:
bad/ and good/
