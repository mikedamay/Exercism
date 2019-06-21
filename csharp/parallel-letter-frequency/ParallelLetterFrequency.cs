using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Xml.Serialization;

public static class ParallelLetterFrequency
{
    public static Dictionary<char, int> Calculate(IEnumerable<string> texts)
    {
        var xx = texts
            .Select(t => t.ToLower())
            .SelectMany(t => t, (t, c) => c)
            .Where(Char.IsLetter)
            .OrderBy(_ => _);
        var xxx = xx.GroupBy(c => c).ToDictionary(g => g.Key, g => g.Count());
        return xxx;
    }
}