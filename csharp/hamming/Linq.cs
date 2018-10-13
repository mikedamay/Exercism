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

        return firstStrand.Zip(secondStrand, (a, b) => (a, b)).Count(p => p.Item1 != p.Item2);
    }

}