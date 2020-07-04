using System;

public static class CentralBank
{
    public static string DisplayDenomination(long @base, long multiplier)
    {
        try
        {
            checked
            {
                return (@base * multiplier).ToString();
            }
        }
        catch (OverflowException)
        {
            return "*** Too Big ***";
        }
    }

    public static string DisplayGDP(float @base, float multiplier)
    {
        float gdp = @base * multiplier;
        if (float.IsInfinity(gdp))
        {
            return "*** Too Big ***";
        }
        else
        {
            return gdp.ToString();
        }
    }

    public static string DisplayAverageSalaryShare(float averageSalary, float GDP)
    {
        var share = (averageSalary / GDP) * 100;
        return share.ToString() + "%";
    }
}
