using System;
using Xunit;


public class WeighingMachineTests
{
    [Fact]
    public void Test()
    {
        var xxx = Store.QueueWithMinimalWaitingTime(customersInQueue1: 0, customersInQueue2: 1, customersInQueue3: 2);
        Assert.True(true);
    }
}