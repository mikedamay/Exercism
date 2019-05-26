using System;
using System.Linq;

public static class Series
{
    public static string[] Slices(string numbers, int sliceLength)
    {
        bool isInvalid() => numbers.Length == 0
                          || sliceLength <= 0
                          || sliceLength > numbers.Length;
        return isInvalid() 
            ? throw new ArgumentException()
            : Enumerable.Range(0, numbers.Length - sliceLength + 1)
                        .Select(n => numbers.Substring(n, sliceLength))
                        .ToArray();
    }
}