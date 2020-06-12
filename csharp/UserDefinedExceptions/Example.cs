using System;
using Microsoft.VisualStudio.TestPlatform.Common.Utilities;

public class CalculationException
{
    
}


public class CalculatorTestHarness
{
    public string Run()
    {
        return string.Empty;
    }
    public void Calculate()
    {
        TestHarness.FakeOp();
    }
}

public static class TestHarness
{
    public static int FakeOp()
    {
        int trouble = Int32.MaxValue;
        checked
        {
            return Int32.MaxValue * trouble;
        }
    }
}
