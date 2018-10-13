using System;
using System.Collections.Generic;
using System.Linq;

public static class Hamming
{
    public static int Distance(string firstStrand, string secondStrand)
    {
        if (firstStrand == null || secondStrand == null || firstStrand.Length != secondStrand.Length)
        {
            throw new ArgumentException();
        }
       
        return GetStrandPairs(firstStrand, secondStrand).Where(p => p.first != p.second).Count();
    }

    private static IEnumerable<(char first, char second)> GetStrandPairs(string firstStrand, string secondStrand)
    {
        for (int ii = 0; ii < firstStrand.Length; ii++)
        {
            yield return (firstStrand[ii], secondStrand[ii]);
        }
    }
}