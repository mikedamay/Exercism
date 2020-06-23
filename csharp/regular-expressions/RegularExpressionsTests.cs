using Xunit;

public class EqualityTests
{
    [Fact]
    public void IsMatch_match()
    {
        var lp = new LogParser();
        Assert.True(lp.IsMatch("[INFO] My Message", "\\[INFO\\]"));
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void IsMatch_no_match()
    {
    }
}