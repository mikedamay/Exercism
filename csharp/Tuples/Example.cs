using System;
using System.Runtime.Serialization;

public class Det
{
    struct mike
    {
        private int abc;
        mike(int a)
        {
            abc = a;
        }
    }
    public static (bool IsSquare, bool IsCube, int Product) DetectPowers(int num1, int num2, int num3)
    {
        var mike = (1, 2);
        var bob = (1, 2);
        var xxx = bob == mike;
        return (true, true, 1);
    }

    public static int ExtractProduct((bool, bool, bool) powersDetected)
    {
        return 1;
    }
    
}
