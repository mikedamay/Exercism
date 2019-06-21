using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Xml.Serialization;

public static class ParallelLetterFrequency
{
    public static Dictionary<char, int> Calculate(IEnumerable<string> texts)
    {
        // both solutions below are slower as PARALLEL for the test cases
        // but faster for larger data sets
        var combinedText = texts.SelectMany(t => t.ToLower());
        var alphabet = Enumerable.Range(0, 255)
                                 .Select(c => (char)c)
                                 .Where(Char.IsLetter)
                                 .Intersect(combinedText);
        return alphabet
            .AsParallel()
            .SelectMany(c => combinedText
                                                .Where(Char.IsLetter)
                        , (c, x) => (c, c == x ? 1 : 0))
            .Where(p => p.Item2 != 0)
            .BuildDictionary(alphabet);
/*
        // slower
        var xx = texts
            .AsParallel()
            .Select(t => t.ToLower())
            .SelectMany(t => t, (t, c) => c)
            .Where(Char.IsLetter)
             .OrderBy(_ => _);
        var xxx = xx.GroupBy(c => c).ToDictionary(g => g.Key, g => g.Count());
        return xxx;
*/
    }
}

internal static class Exts
{
    public static Dictionary<char, int> BuildDictionary(this IEnumerable<(char, int)> source,
        IEnumerable<char> alphabet)
    {
        var dict = alphabet.ToDictionary(a => a, a => 0);
        foreach (var p in source)
        {
            dict[p.Item1]++;
        }

        return dict;
    }
}