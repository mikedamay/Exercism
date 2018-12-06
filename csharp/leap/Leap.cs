public static class Leap
{
    public static bool IsLeapYear(int year) 
      => year % 4 == 0 && year % 100 != 0 || year % 400 == 0;
}
/*
Exercises in Exercism are designed to be run by unit tests. Pay close attention to the instructions 
about running the tests, and follow the link there to the C# language page if you're running into trouble. 
Your submission is expected to return a value rather than outputting it to the console. 
Your initial download should have included a Leap.cs file with a template for you to get started with. 
Once you've got that figured out, go ahead and submit a new solution. :-)
*/

/*
Good solution.

parentheses are optional in this case as operator precedence works in your favour.

You could use an expression bodied member here - also a matter of taste.
 */

/*
There is no requirement for a conditional statement and boolean literals.

Instead of having an if-statement and explicitly returning `true` or `false`, you could also just return the expression in the if-statement.

Consider the following two bits of code, which are functionally equivalent:
```
if (x > 1)
{
    return true;
}

return false;
```
and
```
return x > 1;
```
*/