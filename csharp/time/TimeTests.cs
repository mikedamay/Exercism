using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Threading;
using Xunit;

public class TimeTests
{
    public TimeTests()
    {
        Thread.CurrentThread.CurrentCulture = new CultureInfo("en-US");
    }
    [Fact]
    public void ShowLocalTime()
    {
        var aaa = GetTimeZoneIds();
        var dt = new DateTime(2030, 07, 25, 13, 45, 0);
        var tzi = TimeZoneInfo.Local;
        var offset = tzi.GetUtcOffset(dt);
        Assert.Equal(dt + offset, Appointment.ShowLocalTime(dt));
    }
    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void Schedule_newyork()
    {
        Assert.Equal(new DateTime(2019, 07, 25, 12, 45, 0),
            Appointment.Schedule("7/25/2019 08:45:00", Location.NewYork));
    }

    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void Schedule_london()
    {
        Assert.Equal(new DateTime(2019, 07, 25, 12, 45, 0),
            Appointment.Schedule("7/25/2019 13:45:00", Location.London));
    }

    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void Schedule_paris()
    {
        Assert.Equal(new DateTime(2019, 07, 25, 12, 45, 0),
            Appointment.Schedule("7/25/2019 14:45:00", Location.Paris));
    }

    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void GetAlertTime_early()
    {
        Assert.Equal(new DateTime(2019, 07, 24, 16, 0, 0),
            Appointment.GetAlertTime(new DateTime(2019, 7, 25, 16, 0, 0),
                AlertLevel.Early));
    }
    
    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void GetAlertTime_standard()
    {
        Assert.Equal(new DateTime(2019, 07, 25, 14, 15, 0),
            Appointment.GetAlertTime(new DateTime(2019, 7, 25, 16, 0, 0),
                AlertLevel.Standard));
    }
    
    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void GetAlertTime_late()
    {
        Assert.Equal(new DateTime(2019, 07, 25, 15, 30, 0),
            Appointment.GetAlertTime(new DateTime(2019, 7, 25, 16, 0, 0),
                AlertLevel.Late));
    }
    
    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void DalylightSavingChanged_newyork_active()
    {
        Assert.True(
            Appointment.HasDaylightSavingChanged(new DateTime(2019, 3, 13, 0, 0, 0),
                Location.NewYork));
    }
    
    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void DalylightSavingChanged_newyork_inactive()
    {
        Assert.True(
            Appointment.HasDaylightSavingChanged(new DateTime(2019, 11, 7, 0, 0, 0),
                Location.NewYork));
    }
    
    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void DalylightSavingChanged_newyork_no_change()
    {
        Assert.False(
            Appointment.HasDaylightSavingChanged(new DateTime(2019, 12, 25, 0, 0, 0),
                Location.NewYork));
    }
    
    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void DalylightSavingChanged_london_active()
    {
        Assert.True(
            Appointment.HasDaylightSavingChanged(new DateTime(2019, 4, 1, 0, 0, 0),
                Location.London));
    }
    
    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void DalylightSavingChanged_london_inactive()
    {
        Assert.True(
            Appointment.HasDaylightSavingChanged(new DateTime(2019, 10, 29, 0, 0, 0),
                Location.London));
    }
    
    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void DalylightSavingChanged_london_no_change()
    {
        Assert.False(
            Appointment.HasDaylightSavingChanged(new DateTime(2019, 12, 25, 0, 0, 0),
                Location.London));
    }
    
    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void DalylightSavingChanged_paris_active()
    {
        Assert.True(
            Appointment.HasDaylightSavingChanged(new DateTime(2019, 4, 1, 0, 0, 0),
                Location.Paris));
    }
    
    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void DalylightSavingChanged_paris_inactive()
    {
        Assert.True(
            Appointment.HasDaylightSavingChanged(new DateTime(2019, 10, 29, 0, 0, 0),
                Location.Paris));
    }
    
    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void DalylightSavingChanged_paris_no_change()
    {
        Assert.False(
            Appointment.HasDaylightSavingChanged(new DateTime(2019, 12, 25, 0, 0, 0),
                Location.Paris));
    }

    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void NormalizeDateTime_newyork()
    {
        Assert.Equal( new DateTime(2019, 11, 25, 13, 45, 0),
            Appointment.NormalizeDateTime("11/25/2019 13:45:00",
                Location.NewYork));
    }

    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void NormalizeDateTime_london()
    {
        Assert.Equal( new DateTime(2019, 11, 25, 13, 45, 0),
            Appointment.NormalizeDateTime("25/11/2019 13:45:00",
                Location.London));
    }

    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void NormalizeDateTime_paris()
    {
        Assert.Equal( new DateTime(2019, 11, 25, 13, 45, 0),
            Appointment.NormalizeDateTime("25/11/2019 13:45:00",
                Location.Paris));
    }

    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void NormalizeDateTime_bad()
    {
        Assert.Equal( DateTime.MinValue,
            Appointment.NormalizeDateTime("25/11/2019 13:45:00",
                Location.NewYork));
    }

    private static IList<string> GetTimeZoneIds()
    {
        return TimeZoneInfo.GetSystemTimeZones().Select(tzi => tzi.Id).OrderBy(tzi => tzi).ToList();
    }
}