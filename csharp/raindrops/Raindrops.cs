using System;
using System.Linq;

public static class Raindrops
{
    public static string Convert(int number)
    {
        var list = new int[] {3, 5, 7}.Where(n => n < number).Where(
                n => number % n == 0).Where(nn => nn == 3 || nn == 5 || nn == 7).Append(number)
            .Select(nnn => nnn == 3 ? "Pling" : nnn == 5 ? "Plang" : nnn == 7 ? "Plong" : nnn.ToString())
            .ToArray();
        
        return string.Join(string.Empty, list, 0, Math.Max(list.Length - 1, 1));
    }
}