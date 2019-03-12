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

/*
An alternative approach with LINQ is to use Enumerable.Zip. 
This seems a touch more expressive given the LINQ idiom.

I did a timing comparison on an i7 Windows 10 machine 
repeating one test a billion times:

LINQ Zip - 36 seconds

LINQ Range - 15 seconds

non-LINQ - 10 seconds
*/
/*
Review Points:

Good solution

Discussion Points:

I'm happy to discus a LINQ based solution with you if that is of interest.
 */

/*
Review Points:

Good solution

Discussion Points:

A Zip based LINQ solution takes 3 times as long as a typical non-LINQ solution.

A Range based LINQ solution only takes 50% longer than a typical non-LINQ solution
*/