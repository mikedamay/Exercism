using System;

public class Calculator
{
    public double[] Memory { get; } = new double[256];
    public byte Current { get; private set; }

    public double Multiply(double x, double y)
    {
        double dd = x * y;
        if (!Double.IsFinite(dd))
            // I'm not sure what this covers - may have to add IsNaN etc.
        {
            dd = 0d;
        }
        Memory[Current++] = dd;
        return dd;
    }
}