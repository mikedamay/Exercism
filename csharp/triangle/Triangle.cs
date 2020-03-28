using System;
using System.Runtime.InteropServices.ComTypes;
using System.Security.Cryptography;

public static class Triangle
{
    private enum TriangleType
    {
        NotATriangle = 0,
        Scalene = 1,
        Isosceles = 2,
        Equalateral = 4
    }

    public static bool IsScalene(double side1, double side2, double side3)
        => GetTriangleType(side1, side2, side3).HasFlag(TriangleType.Scalene);

    public static bool IsIsosceles(double side1, double side2, double side3)
        => GetTriangleType(side1, side2, side3).HasFlag(TriangleType.Isosceles);
    public static bool IsEquilateral(double side1, double side2, double side3) 
        => GetTriangleType(side1, side2, side3).HasFlag(TriangleType.Equalateral);

    private static TriangleType GetTriangleType(double side1, double side2, double side3)
    {
        var xxx = (side1, side2, side3) switch
            {
            var (s1, s2, s3) when s1 == 0 && s2 == 0 && s3 == 0 => TriangleType.NotATriangle,
            var (s1, s2, s3) when s1 >= s2 + s3 || s2 >= s1 + s3 || s3 >= s1 + s2 => TriangleType.NotATriangle,
            var (s1, s2, s3) when s1 != s2 && s2 != s3 && s1 != s3 => TriangleType.Scalene,
            var (s1, s2, s3) when s1 != s2 || s2 != s3 || s1 != s3 => TriangleType.Isosceles,
            var (s1, s2, s3) when s1 == s2 && s2 == s3 && s1 == s3 => TriangleType.Equalateral | TriangleType.Isosceles,
            };
        return xxx;
    }
}