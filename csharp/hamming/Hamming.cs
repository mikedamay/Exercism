using System;

public static class Hamming
{
    public static int Distance(string firstStrand, string secondStrand)
    {
        if (firstStrand == null || secondStrand == null || firstStrand.Length != secondStrand.Length)
        {
            throw new ArgumentException();
        }

        int distance = 0;
        for (int ii = 0; ii < firstStrand.Length; ii++)
        {
            distance += firstStrand[ii] != secondStrand[ii] ? 1 : 0;
        }

        return distance;
    }
}