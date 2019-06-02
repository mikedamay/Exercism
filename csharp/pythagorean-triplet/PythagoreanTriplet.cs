using System;
using System.Collections.Generic;

public static class PythagoreanTriplet
{
    public static IEnumerable<(int a, int b, int c)> TripletsWithSum(int sum)
    {
        for (int aa = 3; aa <= sum; aa++)
        {
            for (int bb = aa + 1; bb <= sum - aa; bb++)
            {
                if (aa * bb % 12 != 0)
                    continue;
                for (int cc = bb + 1; cc <= sum - aa - bb; cc++)
                {
                    if (aa + bb + cc == sum &&
                        
                        aa < bb && bb < cc 
                                && aa * aa + bb * bb == cc * cc)
                        yield return (aa, bb, cc);
                }
            }
        }
    }
}