using System;
using System.Collections.Generic;
using System.Linq;

public static class Raindrops_Alternate
{
    public static string Convert(int number)
    {
        Dictionary<int, string> dict = new Dictionary<int, string>()
        {
            [3] = "Pling",
            [5] = "Plang",
            [7] = "Plong",
        };

        var factors = dict
            .Where(kv => number % kv.Key == 0)
            .Select(kv => kv.Value)
            .DefaultIfEmpty(number.ToString());

        return String.Join("", factors);
    }
}