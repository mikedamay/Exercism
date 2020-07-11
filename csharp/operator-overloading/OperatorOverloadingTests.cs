using System;
using Xunit;

public class OperatorOverloadingTests
{
    [Fact]
    public void Equality_true()
    {
        Assert.True(Eq(new Currency(55, "HD"), new Currency(55, "HD")));
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void EQuality_false()
    {
        Assert.False(Eq(new Currency(55, "HD"), new Currency(60, "HD")));
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void EQuality_bad()
    {
        Assert.Throws<ArgumentException>(() => Eq(new Currency(55, "HD"), new Currency(60, "USD")));
    }

    private bool Eq(Currency amountA, Currency amountB)
    {
        return amountA == amountB;
    }
}