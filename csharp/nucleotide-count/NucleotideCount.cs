using System;
using System.Collections.Generic;
using System.Linq;

public static class NucleotideCount
{
    
/*
    private Dictionary<char, int> _counts {get;}
    public NucleotideCount(string sequence)
    {
        if (sequence == null) { throw new ArgumentException(nameof(sequence)); }
//        _counts = MyCount(sequence);
   }

    public IDictionary<char, int> NucleotideCounts => _counts;
*/

    public static IReadOnlyDictionary<char, int> Count(string sequence)
    {
        if (sequence.ToUpper().Except("ACGT").Any())
        {
            throw new ArgumentException(nameof(sequence));
        }
        var arr = new int[91];        // make sure the ASCII characters will find a slot
        foreach (var c in sequence)
        {
            arr[c]++;
        }

        return new List<(char nucleotide, int count)> {('A', arr['A']), ('C', arr['C']), ('G', arr['G']), ('T', arr['T']),}
          .ToDictionary(kv => kv.nucleotide, kv => kv.count);
    }
 }

/*
You should consider returning a read only version 
of the dictionary to the caller.  This will guide 
maintainers in how you expect the dictionary to be used.
*/