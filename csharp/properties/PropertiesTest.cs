using System;
using Xunit;
using Example;

public class WeighingMachineTests
{
    [Fact]
    public void Got_weight_is_set_weight()
    {
        var wm = new WeighingMachine();
        wm.Weight = 77.7f;
        Assert.Equal(77.7, wm.Weight );
    }
    [Fact]
    public void Got_british_weight()
    {
        var wm = new WeighingMachine();
        wm.Weight = 77.7f;
        var expected = new BritishWeight(10, 10, 10);
        Assert.Equal(expected, wm.BritishWeight );
    }
    [Fact]
    public void Got_british_weight_in_pounds()
    {
        var wm = new WeighingMachine();
        wm.Units = Units.Pounds;
        wm.Weight = 77.7f;
        var expected = new BritishWeight(10, 10, 10);
        Assert.Equal(expected, wm.BritishWeight );
    }
    [Fact]
    public void Got_reduced_weight()
    {
        var wm = new WeighingMachine();
        wm.Weight = 100;
        wm.Reduction = 10;
        Assert.Equal(90, wm.Weight );
    }
    [Fact]
    public void Negative_weight_is_invalid()
    {
        var wm = new WeighingMachine();
        Assert.Throws<ArgumentException>(() => wm.Weight = -10);
    }
    
}