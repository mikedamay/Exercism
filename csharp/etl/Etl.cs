using System;
using System.Collections.Generic;
using System.Linq;

public static class Etl
{
    public static Dictionary<string, int> Transform(Dictionary<int, string[]> old)
      => old.SelectMany(kvp => kvp.Value.Select(c => (c.ToLower(), kvp.Key))).ToDictionary(p => p.Item1, p => p.Item2);
}