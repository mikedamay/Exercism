using System;
using System.Collections.Generic;
using System.Linq;

public enum Schedule
{
    Teenth,
    First,
    Second,
    Third,
    Fourth,
    Last
}

public class Meetup
{

    private readonly int month;
    private readonly int year;
    private static readonly TimeSpan OneDay = new TimeSpan(1, 0, 0,0 );
    public Meetup(int month, int year)
    {
        this.month = month;
        this.year = year;
    }

    public DateTime Day(DayOfWeek dayOfWeek, Schedule schedule)
    {
        IEnumerable<DateTime> candidateDays
            = Days()
                .Where(d => d.DayOfWeek == dayOfWeek)
            ;
        switch (schedule)
        {
            case Schedule.First:
                return candidateDays.First();
            case Schedule.Last:
                return candidateDays.Last();
            case Schedule.Second:
                return candidateDays.Skip(1).First();
            case Schedule.Third:
                return candidateDays.Skip(2).First();
            case Schedule.Fourth:
                return candidateDays.Skip(3).First();
            case Schedule.Teenth:
                return candidateDays.First(d => d.Day >= 13 && d.Day <= 19);
            default:
                throw new ArgumentException();
        }
    }

    private IEnumerable<DateTime> Days()
    {
        for (DateTime dt = new DateTime(year, month, 1)
            ; dt.Month == month
            ; dt += OneDay)
        {
            yield return dt;
        }
    }
}