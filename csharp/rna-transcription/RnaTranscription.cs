using System.Collections.Generic;
using System.Linq;

public static class RnaTranscription
{
    public static string ToRna(string nucleotide)
    {
        return new string(nucleotide.Select(c => map[c]));
    }

    private static IReadOnlyDictionary<char, char> map = new Dictionary<char, char>
    {
        { 'C', 'G' },
        { 'G', 'C' },
        { 'T', 'A' },
        { 'A', 'U' }
    };
    
}
/*
Review Points:

Good solution

Discussion Points:

You could use a `ReadOnlyDictionary` to emphasise its role here.

A variant is `string.Concat()` which allows you to avoid a conversion to an array.
*/