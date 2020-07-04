using System;
using Xunit;

public class OverflowTests
{
    [Fact]
    public void DisplayDenomination_good()
    {
        Assert.Equal("10000000", CentralBank.DisplayDenomination(10000L, 1000L));
    }

    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void DisplayDenomination_bad()
    {
        Assert.Equal("*** Too Big ***", CentralBank.DisplayDenomination(10000L, long.MaxValue));
    }

    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void DisplayGDP_good()
    {
        Assert.Equal("5550000", CentralBank.DisplayGDP(555f, 10000f));
    }

    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void DisplayGDP_bad()
    {
        Assert.Equal("*** Too Big ***", CentralBank.DisplayGDP(555f, float.MaxValue));
    }
}