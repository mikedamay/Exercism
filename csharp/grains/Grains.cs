using System;
using System.Collections.Generic;
using System.Linq;

public static class Grains
{
    public static ulong Square(int n)
        => n <= 0 || n > 64 ? throw new ArgumentOutOfRangeException("invalid input of 0") : 1UL << (n - 1);
    public static ulong Total()
    {
        return Enumerable.Range(1, 64).Select(n => Square(n)).Aggregate((m, n) => m + n);
    }

}