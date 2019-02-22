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
