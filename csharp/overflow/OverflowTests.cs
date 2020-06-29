using System;
using Xunit;

public class OverflowTests
{
    [Fact]
    public void Multiply_bad()
    {
        var calculator = new Calculator();
        Assert.Equal(0.0d, calculator.Multiply(Double.MaxValue, Double.MaxValue));
    }

    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void Store_257()
    {
        var calculator = new Calculator();
        for (int ii = 0; ii <= 257; ii++)
        {
            calculator.Multiply((double)ii, 100d);
        }
        Assert.Equal(25600, calculator.Memory[0]);
    }
}