using System.Linq;
using System.Collections.Generic;

public static class SumOfMultiples_NonLinq
{
    public static int Sum(IEnumerable<int> multiples, int max)
    {
        var results = new HashSet<int>();
        foreach (var multiple in multiples)
        {
            // reversed direction to avoid int overflow
            for (int i = (max - 1) / multiple * multiple; i > 0; i -= multiple)
            {
                results.Add(i);
            }
        }

        return results.Sum();
    }
}