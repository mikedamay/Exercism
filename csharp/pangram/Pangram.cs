using System;
using System.Linq;
using System.Text;

public static class Pangram
{
    public static bool IsPangram(string input)
    {
/*
        // I prefer alphabet as a literal
        string alphabet()
        {
            StringBuilder sb = new StringBuilder();
            return Enumerable
                .Range(97, 26)
                .Select(n => (char)n)
                .Aggregate(sb, (chs, ch) => chs.Append(ch))
                .ToString();
            
        }
*/

        return !"abcdefghijklmnopqrstuvwxyz".Except(input.Replace(" ", string.Empty).ToLower()).Any();
    }
}
