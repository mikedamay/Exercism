using System;
using Xunit;

public class UserDefinedExceptionsTests
{
    [Fact]
    public void Call_Calculate()
    {
        var cth = new CalculatorTestHarness();

        Assert.Throws<CalculationException>( () => cth.Calculate(123) );
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void Call_Run_Calculate_with_0()
    {
        var cth = new CalculatorTestHarness();
        Assert.Equal("Calculate failed for a zero value", cth.Run("Calculate", 0));
    }
    
    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void Call_Run_Calculate_with_non_0()
    {
        var cth = new CalculatorTestHarness();
        Assert.Equal("Calculate failed for a non-zero value", cth.Run("Calculate", 123));
    }
    
    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void Call_Run_HandleInt()
    {
        var cth = new CalculatorTestHarness();
        Assert.Equal("HandleInt failure", cth.Run("HandleInt", 123));
    }
}