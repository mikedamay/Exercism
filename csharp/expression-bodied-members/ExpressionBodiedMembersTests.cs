using Xunit;

public class ExpressionBodiedMembersTests
{
    [Fact]
    public void GetReading()
    {
        var ws = new WeatherStation();
        ws.AcceptReading(new Reading(20m, 25m, 0.01m));
        decimal[] expected = {20, 25, 0.01m};
        decimal[] actual = {ws.LatestTemperature, ws.LatestTemperature, ws.LatestRainfall};
        Assert.Equal(expected, actual);
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void HasHistory_no()
    {
        var ws = new WeatherStation();
        ws.AcceptReading(new Reading(20m, 25m, 0.01m));
        Assert.False(ws.HasHistory);
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void HasHistory_yes()
    {
        var ws = new WeatherStation();
        ws.AcceptReading(new Reading(20m, 25m, 0.01m));
        ws.AcceptReading(new Reading(21m, 25m, 0.00m));
        Assert.True(ws.HasHistory);
    }
}