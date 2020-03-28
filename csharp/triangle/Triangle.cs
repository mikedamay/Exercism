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
        => (side1, side2, side3) switch
            {
            _ when side1 >= side2 + side3 || side2 >= side1 + side3 || side3 >= side1 + side2 
            => TriangleType.NotATriangle,
            _ when side1 != side2 && side2 != side3 && side1 != side3 => TriangleType.Scalene,
            _ when side1 != side2 || side2 != side3 => TriangleType.Isosceles,
            _ => TriangleType.Equalateral | TriangleType.Isosceles,
            };
}