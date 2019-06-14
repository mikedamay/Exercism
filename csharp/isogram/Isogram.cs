using System;
using System.Linq;

public static class Isogram
{
    public static bool IsIsogram(string word)
    {
        return !word.Select(c => Char.ToLower(c))
            .Where(c => Char.IsLetter(c))
            .OrderBy(c => c)
            .GroupBy(c => c)
            .Any(g => g.Count() > 1);
    }
}

/*
Review Points:

Good solution

Discussion Points:

You might want to think about a LINQ based solution (or you might not see the point in which case let me know).  Use of `Distinct()` or `GroupBy()` indicate two different ways to approach this.
*/