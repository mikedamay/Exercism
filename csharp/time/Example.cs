using System;
using System.Diagnostics;
using System.Globalization;

public enum Location
{
    NewYork,
    London,
    Paris
}

public enum AlertLevel
{
    Early,
    Standard,
    Late
}

public static class Appointment
{
    public static DateTime ShowLocalTime(DateTime dt)
    {
        return dt.ToLocalTime();
    }

    public static DateTime Schedule(string appointmentDateDescription, Location location)
    {
        DateTime dt = DateTime.Parse(appointmentDateDescription);
        DateTime local = TimeZoneInfo.ConvertTimeBySystemTimeZoneId(dt, GetTimeZoneId(location));
        return local;
    }

    public static DateTime GetAlertTime(DateTime appointment, AlertLevel alertLevel)
    {
        TimeSpan noticePeriod = new TimeSpan();

        switch (alertLevel)
        {
            case AlertLevel.Early:
                noticePeriod = new TimeSpan(1, 0, 0, 0);
                break;
            case AlertLevel.Standard:
                noticePeriod = new TimeSpan(1, 45, 0);
                break;
            case AlertLevel.Late:
                noticePeriod = new TimeSpan(0, 30, 0);
                break;
        }

        return appointment - noticePeriod;
    }

    public static bool HasDaylightSavingChanged(DateTime dt, Location location)
    {
        DateTime dtPrevious = dt.AddDays(-7);
        string tzid = string.Empty;
        TimeZoneInfo tzi = TimeZoneInfo.FindSystemTimeZoneById(GetTimeZoneId(location));
        return tzi.IsDaylightSavingTime(dtPrevious) != tzi.IsDaylightSavingTime(dt);
    }

    public static DateTime NormalizeDateTime(string dtStr, Location location)
    {
        return DateTime.Parse(dtStr, LocationToCulture(location));
    }

    private static CultureInfo LocationToCulture(Location location) =>
        location switch
        {
            Location.NewYork => new CultureInfo("en-US"),
            Location.London => new CultureInfo("en-GB"),
            Location.Paris => new CultureInfo("fr-FR")
        };

#if Windows
    private static string GetTimeZoneId(Location location) =>
        location switch

        {
            Location.NewYork => "Eastern Standard Time",
            Location.London => "GMT Standard Time",
            Location.Paris => "W. Europe Standard Time"
        };
#else    
    private static string GetTimeZoneId(Location location) =>
        location switch

        {
            Location.NewYork => "America/New_York",
            Location.London => "America/New_York",
            Location.Paris => "America/New_York"
        };
#endif    
    
}
