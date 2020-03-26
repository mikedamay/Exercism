using System;
using Xunit;
using Example;

public class WeighingMachineTests
{
    [Fact]
    public void Set_weight_and_get_weight()
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
    public void Get_US_display_weight()
    {
        var wm = new WeighingMachine();
        wm.InputWeight = 60m;
        Assert.Equal((132, 4), (wm.USDisplayWeight.Pounds, wm.USDisplayWeight.Ounces) );
    }
    [Fact]
    public void Input_pounds_and_get_US_display_weight()
    {
        var wm = new WeighingMachine();
        wm.Units = Units.Pounds;
        wm.InputWeight = 175.5m;
        Assert.Equal((175, 8), (wm.USDisplayWeight.Pounds, wm.USDisplayWeight.Ounces) );
    }
    [Fact]
    public void Apply_tare_adjustment_and_get_display_weight()
    {
        var wm = new WeighingMachine();
        wm.InputWeight = 100;
        wm.TareAdjustment = 10;
        Assert.Equal(90, wm.DisplayWeight );
    }
    
}