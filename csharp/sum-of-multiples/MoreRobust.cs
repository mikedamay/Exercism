using System;
using System.Collections.Generic;
using System.Linq;

public static class SumOfMultiples
{
    public static int Sum(IEnumerable<int> multiples, int max)
    {        
        try
        {
            return multiples
                .Select(input => input <= 0 ? throw new Exception("stuff") : input)
                .SelectMany(input => Enumerable
                    .Range(1, (max-1) / input)
                    .Select( step => step * input))
                .Distinct()
                .Sum();
        }
        catch(OverflowException)
        {
            throw new InvalidOperationException("Sum of multiples resulted in a value larger than an Int32.");
        }
    }
}
/*
This works well for the tests but try to broaden the range of inputs that the routine can handle e.g.
```
Assert.Equal(1_999_999_999, SumOfMultiples.Sum(new[] { 1_999_999_999 }, 2_000_000_000));
```
*/

/*
This is a case where you have to consider the algorithm you are using.

The problem is that you are doing a moderately heavy task (iterating over the multiples) for every number between 1 and max.  In the above case this is 2 biillion operations.

One approach is to put the multiples in the "outer loop" and then you can be more economical about the processing for that particular multiple.

Have a look at `Enumerable.SelectMany()` which can help combine each multiple with testing each use of the multiple.

If all else fails then use explicit `for` loops as a starting point.
*/
/*
The exercise is slightly unsatisfactory in that a 
performant bullet proof solution is not possible 
without changing the signature of `Sum`to return a 
long and a radical change to the algorithm to pre-
calculate where results would be duplicated.
*/
/*
I imagine you would be happier without the `foreach`.  The key LINQ construct for this is `SelectMany`. 

''`
multiples.SelectMany(collection, (item-from-collection, mulitple) => do something with the two values)
``` 
can provide a non-unique enumerable of values based on the multiples.

The first parameter to `SelectMany` is a collection.  The second parameter is a function which is handed (by SelectMany) each item from the collection together with the current item from the original object on which `SelectMany` was called.  in this case every muultiple is paired up with every item from "collection".

*/

/*
This exercise suffers from the problem that a simple solution such as looping through all possible numbers less than max and testing against each multiple passed in will either cause overflow exceptions or be unnecessarily slow for very many scenarios. 

The case I provided is one such instance.  But this case can be easily accommodated by avoiding examining each number between 1 and max (after all you only care about numbers are a multiple of "multiple").

Other cases such as having max of `1_999_999_999` and an array of multiples comprising every number between 1 and one billion would throw an out-of-memory exception long before it would throw the inevitable integer overflow exception.

Because of the above my comment is necessarily vague.  I can't say "fix it" because there isn't really a fix but I can encourage students to be aware that the simple solution is a very partial solution and that there is sometimes a very significant trade-off between the elegance and readability provided by simple LINQ solution and performance.

*/
