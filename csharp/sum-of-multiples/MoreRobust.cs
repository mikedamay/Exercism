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