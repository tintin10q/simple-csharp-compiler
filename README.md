# Simple C# compiler
A simple c# compiler written in haskell. 

It can compile one class for a simple [stack machine](https://www.cs.uu.nl/docs/vakken/b3tc/SSM/). A java implementation of the stack machine is included here for convience. I did not make/design this stack machine or this implementation only this compiler for it. 

Features include methods with parameters, recursion, while loops, for loops,  local and global variables of types bools, ints and chars, lazy evaluation of conditional logic and single line comments. The compiler can only compile a single class. The entry point of the program is the main method of a class. 

## Operators 

The follwing operators are implemented: 

-  * 
-  / 
-  % 

-  + 
-  -

-  <=
-  <
-  >= 
-  >

-  == 
-  !=

-  ^

-  &&

-  ||

-   =

The operators follow priority according to the official [oder of operations reference of C#](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/operators). Left and right associative operators are handled appropriatly. 


## Build in functions

There are 4 build in functions:

- print - Print numbers 
- printc - Print unicode characters
- input - Input a single number
- inputc - Input a single unicode character 


## Example class
An example class the compiler can compile is below this:

```c#
class Hello
{
    int globalvar = 10;

    void main() # Entry point 
    {
      printc('g','l','o','b','a','l');
      print(globalvar);
      print(fac(10));
    }

    int fac(int x)
    {
        int r; int t;
        t=1; r=1;
        while (t<=x)
        {
            r = r*t;
            t = t+1;
        }
        return r;
    }
}
```

