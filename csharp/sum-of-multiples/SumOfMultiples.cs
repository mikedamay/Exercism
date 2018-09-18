using System.Linq;
using System.Collections.Generic;

public static class SumOfMultiples_Main
{
    public static int Sum(IEnumerable<int> multiples, int max)
        => multiples.SelectMany(
            multiple => Enumerable.Range(1, (max - 1) / multiple).Select(numTimes => numTimes * multiple)
            , (numtimes, multiple) => multiple).Distinct().Sum();
}