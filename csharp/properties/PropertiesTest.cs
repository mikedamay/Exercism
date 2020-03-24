using System;
using Xunit;
using Example;

public class WeighingMachineTests
{
    [Fact]
    public void Got_weight_is_set_weight()
    {
        var wm = new WeighingMachine();
        wm.InputWeight = 77.7f;
        Assert.Equal(77.7, wm.InputWeight, 3 );
    }
    [Fact]
    public void Got_british_weight()
    {
        var wm = new WeighingMachine();
        wm.InputWeight = 77.7f;
        Assert.Equal((12, 3, 4), (wm.BritishWeight.Stones, wm.BritishWeight.Pounds, wm.BritishWeight.Ounces) );
    }
    [Fact]
    public void Got_british_weight_in_pounds()
    {
        var wm = new WeighingMachine();
        wm.Units = Units.Pounds;
        wm.InputWeight = 175f;
        Assert.Equal((12, 7, 0), (wm.BritishWeight.Stones, wm.BritishWeight.Pounds, wm.BritishWeight.Ounces) );
    }
    [Fact]
    public void Got_reduced_weight()
    {
        var wm = new WeighingMachine();
        wm.InputWeight = 100;
        wm.VanityFactor = 10;
        Assert.Equal(90, wm.DisplayWeight );
    }
    [Fact]
    public void Negative_weight_is_invalid()
    {
        var wm = new WeighingMachine();
        Assert.Throws<ArgumentException>(() => wm.InputWeight = -10);
    }
    
}