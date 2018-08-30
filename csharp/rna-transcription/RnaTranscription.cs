using System.Collections.Generic;
using System.Linq;

public static class RnaTranscription
{
    public static string ToRna(string nucleotide)
    {
        return string.Concat(nucleotide.Select(c => map[c]));
    }

    private static IReadOnlyDictionary<char, char> map = new Dictionary<char, char>
    {
        { 'C', 'G' },
        { 'G', 'C' },
        { 'T', 'A' },
        { 'A', 'U' }
    };
    
}
