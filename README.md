# Simple C# compiler
A simple c# compiler written in haskell. 

It can compile one class for a simple [stack machine](https://www.cs.uu.nl/docs/vakken/b3tc/SSM/).

It does classes like this:

```c#
class Hello
{
    int globalvar = 10;

    void main()
    {
      printcNl('g','l','o','b','a','l');
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
