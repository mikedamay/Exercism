using System;
using System.Linq;
using System.Collections.Generic;

public class SumOfMultiples
{
    public static int Sum(IEnumerable<int> multiples, int max)
    {
        multiples = multiples.Where(f => f != 0);

        HashSet<int> sumFactors = new HashSet<int>();

        foreach(var multiple in multiples)
        {
            for (int i = (max / multiple); i > 0; i--)
            {
                int number = multiple * i;
                if (number < max)
                {
                    sumFactors.Add(number);
                }
            }
        }

        return sumFactors.Sum();
    }
}