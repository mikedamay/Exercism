using System;
using System.ComponentModel.DataAnnotations;
using System.Threading;
using Xunit;

public class TimeTests
{
    [Fact]
    public void ShowLocalTime()
    {
        var dt = new DateTime(2030, 07, 25, 13, 45, 0);
        var tzi = TimeZoneInfo.Local;
        var offset = tzi.GetUtcOffset(dt);
        Assert.Equal(dt + offset, Appointment.ShowLocalTime(dt));
    }
    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void Schedule_newyork()
    {
        Assert.Equal(new DateTime(2030, 07, 25, 8, 45, 0),
            Appointment.Schedule("7/25/2030 13:45:00", Location.NewYork));
    }

    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void Shecule_london()
    {
        Assert.Equal(new DateTime(2030, 07, 25, 13, 45, 0),
            Appointment.Schedule("7/25/2030 13:45:00", Location.London));
    }

    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void Shecule_paris()
    {
        Assert.Equal(new DateTime(2030, 07, 25, 14, 45, 0),
            Appointment.Schedule("7/25/2030 13:45:00", Location.Paris));
    }
    /*
    private static IList<string> GetTimeZoneIds()
    {
        return TimeZoneInfo.GetSystemTimeZones().Select(tzi => tzi.Id).ToList();
    }
*/
}