using System;
using System.Collections;
using System.Linq;
using System.Collections.Generic;

internal static class SumOfMultiples_BulletProof
{
    internal static int Sum(IEnumerable<int> multiplesArg, int max)
    {
        int tot = SumEx(multiplesArg.Where(m => m < max));
        tot -= SumEx(MultipleCombinations(multiplesArg.Where(m => m < max)));
        return tot;

        int SumEx(IEnumerable<int> multiples)
        {
            int agg = 0;
            foreach (var num in multiples)
            {
                // Clamp larger data sets to avoid memory issues
                if (max / num > 65536)
                {
                    throw new InvalidOperationException("There was a problem summing the multiples.");
                }

                if (num < max)
                {
                    var highest = (max - 1) / num;
                    var numTimes = highest * (highest + 1) / 2;
                    agg += numTimes * num;
                }
            }

            return agg;
        }

/*
            hs.UnionWith(Enumerable.Range(1, (max / num))
                .Where(i => i * num < max)
                .Select(i => i * num)
                .ToHashSet());
*/

    }

    private static IEnumerable<int> MultipleCombinations(IEnumerable<int> multiples)
    {
        var combos = multiples.SelectMany(m => multiples.Where(m2 => m > m2), (a, b) => a*b).ToList();
        var list = combos.ToList();
        return list;
    }
}

public class SummationOverloadExpetion : Exception
{
    public SummationOverloadExpetion()
    {
    }

    public SummationOverloadExpetion(string message)
        : base(message)
    {
    }

    public SummationOverloadExpetion(string message, Exception inner)
        : base(message, inner)
    {
    }
}