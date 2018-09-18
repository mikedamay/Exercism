using System.Collections.Generic;
using System.Linq;

public static class SumOfMultiples_TweakedRange
{
    public static int Sum(IEnumerable<int> multiples, int max)
    {
        return multiples.SelectMany(multiple => GenerateMultiples(multiple, max - 1, multiple))
          .Distinct().Sum();
    }

    public static IEnumerable<int> GenerateMultiples(int min, int max, int step)
    {
        // reversed direction to avoid int overflow
        for (int i = max / step * step; i >= min; i -= step)
        {
            yield return i;
        }
    }
}

