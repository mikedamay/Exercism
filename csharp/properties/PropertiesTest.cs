using System;
using Xunit;
using Example;

public class WeighingMachineTests
{
    [Fact]
    public void Got_weight_is_set_weight()
    {
        var wm = new WeighingMachine();
        wm.InputWeight = 60m;
        Assert.Equal(60m, wm.InputWeight, 3 );
    }
    [Fact]
    public void Negative_weight_is_invalid()
    {
        var wm = new WeighingMachine();
        Assert.Throws<ArgumentException>(() => wm.InputWeight = -10);
    }
    
    [Fact]
    public void Got_british_weight()
    {
        var wm = new WeighingMachine();
        wm.InputWeight = 60m;
        Assert.Equal((9, 6, 4.43520m), (wm.BritishWeight.Stones, wm.BritishWeight.Pounds, wm.BritishWeight.Ounces) );
    }
    [Fact]
    public void Got_british_weight_in_pounds()
    {
        var wm = new WeighingMachine();
        wm.Units = Units.Pounds;
        wm.InputWeight = 175m;
        Assert.Equal((12, 7, 0), (wm.BritishWeight.Stones, wm.BritishWeight.Pounds, wm.BritishWeight.Ounces) );
    }
    [Fact]
    public void Apply_vanity_factor()
    {
        var wm = new WeighingMachine();
        wm.InputWeight = 100;
        wm.VanityFactor = 10;
        Assert.Equal(90, wm.DisplayWeight );
    }
    
}