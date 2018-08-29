using System;
using System.Collections.Generic;
using System.Linq;

public static class SumOfMultiples
{
    public static int Sum(IEnumerable<int> inputs, int max)
        => inputs.SelectMany(
            input => Enumerable.Range(1, (max - 1) / input).Select(numTimes => numTimes * input)
            , (numtimes, multiple) => multiple).ToHashSet().Sum();
}