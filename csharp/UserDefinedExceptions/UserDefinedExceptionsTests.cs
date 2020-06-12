using System;
using Xunit;
using Xunit.Sdk;

public class UserDefinedExceptionsTests
{
    [Fact]
    public void Call_fakeOp()
    {
        var cth = new CalculatorTestHarness();

        Assert.Throws<OverflowException>( () => cth.Calculate()
        );
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void Call_calculate()
    {
        var cth = new CalculatorTestHarness();
        Assert.Equal("Calculate: operation failed in FakeOp", cth.Run());
    }
}