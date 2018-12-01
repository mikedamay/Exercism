using System;
using System.Linq;
using System.Text;
using Newtonsoft.Json.Serialization;

public static class Hamming
{   public static int Distance(string firstStrand, string secondStrand)
    {

        if (firstStrand == null || secondStrand == null || firstStrand.Length != secondStrand.Length)
        {
            throw new ArgumentException();
        }
    
        // LINQ solution             // 40.0 secs
        var s1 = firstStrand;        // 22.8 secs  top end 2016 IMAC
        var s2 = secondStrand;
//        var s1 = firstStrand.ToCharArray();    // 29.6 secs
//        var s2 = secondStrand.ToCharArray();
//        var s1 = new StringBuilder(firstStrand);    // 33 secs
//        var s2 = new StringBuilder(secondStrand);

        int distance = 0;
        for (int ii = 0; ii < s1.Length; ii++)
        {
            distance += s1[ii] != s2[ii] ? 1 : 0;
        }

        return distance;
    }
}
/*
var dist = s1
    .Select((ch, idx) => new {ch, idx})
    .Join(s2
            .Select((ch, idx) => new {ch, idx}), first => first.idx, second => second.idx
            , (first, second) => first.ch == second.ch ? 0 : 1)
    .Sum();

*/