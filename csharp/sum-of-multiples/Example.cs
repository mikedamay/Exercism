using System.Collections.Generic;
using System.Linq;

public static class SumOfMultiples_Example
{
    public static int Sum(IEnumerable<int> multiples, int max)
    {
        return Enumerable.Range(1, max - 1)
            .Where(i => multiples.Any(m => i % m == 0))
            .Sum();
    }
}

