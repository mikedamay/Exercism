using System;
using System.Linq;

public static class Grains
{
    private const int MIN = 1, MAX = 64;
    
    public static ulong Square(int n)
        => n < MIN || n > MAX ? ThrowInvalid() : TwoToThe(n - 1);
    public static ulong Total()
        => Enumerable
            .Range(MIN, MAX)
            .Select(n => Square(n))
            .Aggregate((allSquares, square) => allSquares + square);

    private static ulong ThrowInvalid() => throw new ArgumentOutOfRangeException("invalid input of 0");
    private static ulong TwoToThe(int n) => 1UL << n;
}