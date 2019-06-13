using System;
using System.Linq;

public static class LargestSeriesProduct
{
    public static long GetLargestProduct(string digits, int span) 
    {
        int DigitToInt(char digit) =>  Char.IsDigit(digit) ? digit - '0' : throw new ArgumentException();
        
        if (span < 0 || span != 0 && span > digits.Length) throw new ArgumentException();

        if (span == 0) return 1;

        return Enumerable.Range(0, digits.Length - span + 1)
            .SelectMany(n => digits.Substring(n, span), (n, c) => (n, DigitToInt(c)))
            .GroupBy(p => p.Item1).Select( g => g
            .Aggregate((acc, p) => (p.Item1, acc.Item2 * p.Item2)).Item2).Max();

    }
}