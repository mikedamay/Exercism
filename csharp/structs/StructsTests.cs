using System;
using Xunit;

public struct cl
{
    
}

public class StructsTests
{
    private static readonly Random rand = new Random(1234567);
    [Fact]
    public void CheckLastClaim()
    {
        cl xx = new cl();
    }

    private Coord CreateRandomCoord()
    {
        return new Coord(
            (ushort) rand.Next(0, 65536), (ushort) rand.Next(0, 65536));
    }

    private Plot CreateRandomPlot()
    {
        return new Plot(
            CreateRandomCoord(),
            CreateRandomCoord(),
            CreateRandomCoord(),
            CreateRandomCoord()
        );
    }
    // 5.4 seconds without custom Equals, 4.3 seconds with custom Equals
    // on barely loaded top-end IMac
    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void SpeedTest()
    {
        var claimsHandler = new ClaimsHandler();
        for (int ii = 0; ii < 10_000_000; ii++)
        {
            claimsHandler.StakeClaim(CreateRandomPlot());
        }
    }

    

}    