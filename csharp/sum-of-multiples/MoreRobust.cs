using System;
using System.Collections.Generic;
using System.Linq;

public static class SumOfMultiples_MoreRobust
{
    public static int Sum(IEnumerable<int> multiples, int max)
    {        
        try
        {
            return multiples
                .Select(m => m <= 0 ? throw new Exception("stuff") : m)
                .SelectMany(m => Enumerable
                    .Range(1, (max-1) / m)
                    .Where(x => x < max)
                    .Select( x => m * x))
                .Distinct()
                .Sum();
        }
        catch(OverflowException)
        {
            throw new InvalidOperationException("Sum of multiples resulted in a value larger than an Int32.");
        }
    }
}