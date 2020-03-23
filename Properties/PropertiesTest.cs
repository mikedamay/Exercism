using System.ComponentModel.DataAnnotations;
using Xunit;

public class WeighingMachineTests
{
    public void WeightTest()
    {
        var wm = new WeighingMachine();
        wm.Weight = 77.7f;
        Assert.Equal(77.7, wm.Weight );
    }
}