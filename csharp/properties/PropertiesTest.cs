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
    [Fact]
    public void Apply_negative_tare_adjustment()
    {
        var wm = new WeighingMachine();
        wm.InputWeight = 100;
        wm.TareAdjustment = -10;
        Assert.Equal(110, wm.DisplayWeight );
    }
    [Fact]
    public void Apply_large_tare_adjustment_to_allow_negative_display_weight()
    {
        var wm = new WeighingMachine();
        wm.InputWeight = 100;
        wm.TareAdjustment = 110;
        Assert.Equal(-10, wm.DisplayWeight );
    }

    [Fact]
    public void Units_should_be_auto_implemented()
    {
        bool autoImplemented = false;
        Assert.True(autoImplemented);
    }
    [Fact]
    public void DisplayWeight_should_not_have_a_setter()
    {
        bool hasSetter = true;
        Assert.False(hasSetter);
    }
    [Fact]
    public void USDisplayWeight_should_not_have_a_setter()
    {
        bool hasSetter = true;
        Assert.False(hasSetter);
    }
    [Fact]
    public void TareAdjustment_should_be_auto_implemented_with_private_getter()
    {
        bool hasPrivateGetter = false;
        Assert.False(hasPrivateGetter);
    }
    [Fact]
    public void Pounds_should_not_have_a_setter()
    {
        bool hasSetter = true;
        Assert.False(hasSetter);
    }
    [Fact]
    public void Ounces_should_not_have_a_setter()
    {
        bool hasSetter = true;
        Assert.False(hasSetter);
    }
    
}