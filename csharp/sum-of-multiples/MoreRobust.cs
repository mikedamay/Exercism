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