using System;
using System.Data;
using System.Diagnostics;
using System.Runtime.CompilerServices;

public static class RealNumberExtension
{
    public static double Expreal(this int realNumber, RationalNumber r)
    {
        return Math.Pow(Math.Pow(realNumber, r.numerator),  1.0/(double)r.denominator);
    }
}

public struct RationalNumber
{
    public int numerator;
    public int denominator;
    public RationalNumber(int numerator, int denominator)
    {
        (this.numerator, this.denominator) = Reduce(numerator, denominator);
    }
    private RationalNumber((int numerator, int denominator) r)
    {
        (this.numerator, this.denominator) = r;
    }

    public RationalNumber Add(RationalNumber r)
    {
        return new RationalNumber(Reduce(this.numerator * r.denominator + r.numerator * this.denominator, this.denominator * r.denominator));
    }

    public static RationalNumber operator +(RationalNumber r1, RationalNumber r2)
    {
        return r1.Add(r2);
    }

    public RationalNumber Sub(RationalNumber r)
    {
        return new RationalNumber(Reduce(this.numerator * r.denominator - r.numerator * this.denominator, this.denominator * r.denominator));
    }

    public static RationalNumber operator -(RationalNumber r1, RationalNumber r2)
    {
        return r1.Sub(r2);
    }

    public RationalNumber Mul(RationalNumber r)
    {
        return new RationalNumber(Reduce(this.numerator * r.numerator, this.denominator * r.denominator));
    }

    public static RationalNumber operator *(RationalNumber r1, RationalNumber r2)
    {
        return r1.Mul(r2);
    }

    public RationalNumber Div(RationalNumber r)
    {
        return new RationalNumber(Reduce(this.numerator * r.denominator, r.numerator * this.denominator));
    }

    public static RationalNumber operator /(RationalNumber r1, RationalNumber r2)
    {
        return r1.Div(r2);
    }

    public RationalNumber Abs()
    {
        return new RationalNumber(Reduce(numerator < 0 ? -numerator : numerator, denominator < 0 ? -denominator : denominator));
    }

    public RationalNumber Reduce()
    {
        return new RationalNumber(Reduce(this.numerator, this.denominator));
    }

    public RationalNumber Exprational(int power)
    {
        return new RationalNumber(Reduce((int)Math.Pow(this.numerator, Math.Abs(power)), (int)Math.Pow(this.denominator, Math.Abs(power))));
    }

    public double Expreal(int baseNumber)
    {
        return Math.Pow(Math.Abs(numerator), baseNumber) / Math.Pow(Math.Abs(denominator), baseNumber);
    }

    private static (int numerator, int denominator) Reduce(int numeratorArg, int denominatorArg)
    {
        var gcd = GreatestCommonDivisor(numeratorArg, denominatorArg);
        return (numeratorArg / gcd, denominatorArg / gcd);
    }

    private static int GreatestCommonDivisor(int a, int b)
    {
        return GreatestCommonDivisorR(a > b ? (a, b) : (b, a));
        int GreatestCommonDivisorR((int larger, int smaller) r)
        {
            if (r.smaller == 0)
                return r.larger;

            var remainder = r.larger % r.smaller;
            return GreatestCommonDivisorR((r.smaller, remainder));
        }
    }
}