using System;
using System.Linq;

public static class ArmstrongNumbers
{
    public static bool IsArmstrongNumber(int number)
    {

        string ns = number.ToString();
        uint exponent = (uint)ns.Length;
        return number == ns.Select(c => IntPow(c - 0x30, exponent)).Aggregate((m, n) => m + n);
    }
    private static int IntPow(int x, uint pow)
    {
        int res = 1;
        for (uint ii = 0; ii < pow; ii++)
        {
            res *= x;
        }

        return res;
    }
}