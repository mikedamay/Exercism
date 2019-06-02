using System;
using System.Collections.Generic;
using System.Linq;

public static class PythagoreanTriplet
{
    public static IEnumerable<(int a, int b, int c)> TripletsWithSum(int sum)
    {
        var maxA = sum / 3;
        var maxB = sum / 2;

        return Enumerable.Range(1, maxA)
            .SelectMany(a => Enumerable.Range(a, maxB)
                .Select(b => (a, b, sum - a - b))
                .Where(IsPythagorean));
    }

    private static Func<(int a, int b, int c), bool> IsPythagorean = t => (t.a * t.a) + (t.b * t.b) == (t.c * t.c);
}