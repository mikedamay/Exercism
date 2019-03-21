using System;
using System.Linq;
using System.Text;

public static class Pangram
{
    public static bool IsPangram(string input)
    {
/*
        // I prefer alphabet as a literal
        string alphabet()
        {
            StringBuilder sb = new StringBuilder();
            return Enumerable
                .Range(97, 26)
                .Select(n => (char)n)
                .Aggregate(sb, (chs, ch) => chs.Append(ch))
                .ToString();
            
        }
*/

        return !"abcdefghijklmnopqrstuvwxyz".Except(input.Replace(" ", string.Empty).ToLower()).Any();
    }
}
/*
On mutation.

Firstly there is nothing problematic about your solution to this specific exercise.  My comments address a general approach to coding.  These sort of "principles" become significant in large code bases and are of negligible significance in small snippets of code.

A variable is said to mutate if it can take more than one value in the lifetime of a program.  e.g.
```
// mutating version
int myval = 5;
if (high) myval = 10;
else myval = 0;
Console.WriteLine(myVal)

// non-mutating version
Console.WriteLine( high ? 10 : 0);
```
Intuitively and empirically we see that the presence of mutating variables makes it more difficult to reason about the code to make mistakes about what the value of a variable might be and therefore what the behaviour of the program might be at any point in time.

In the case of your solution you are mutating `mark`, `index` and less problematifally `i`.

You will see from the community solutions that there are a number of solutions that avoid any such mutation.

Please let me know if you further clarification would be helpful.
*/