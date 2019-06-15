using System;

public readonly struct Clock
{
    private readonly int hours, minutes;
    public Clock(int hours, int minutes)
    {
        this.hours = hours;
        this.minutes = minutes;
    }

    public int Hours
    {
        get
        {
            var hours = CalculateHours();
            var minutes = CalculteMinutes();
            if (minutes < 0)
            {
                hours = (((24 + hours) * 60 + minutes) / 60) % 24;
            }
            return hours < 0 ? 24 + hours : hours;
        }
    }

    public int Minutes
    {
        get
        {
            var minutes = CalculteMinutes();
            if (minutes < 0)
            {
                minutes = 60 + minutes;
            }
            return minutes;
        }
    }

    private int HoursToMinutes(int hours, int minutes) => hours * 60 + minutes;

    private int CalculateHours(int hours, int minutes) => ((HoursToMinutes(hours, minutes) / 60) + 24) % 24;

    private int CalculteMinutes(int hours, int minutes) => HoursToMinutes(hours, minutes) % 60;

    public Clock Add(int minutesToAdd) => new Clock(Hours, Minutes + minutesToAdd);

    public Clock Subtract(int minutesToSubtract) => new Clock(Hours, Minutes - minutesToSubtract);

    private (int, int) Normalise(int hours, int minutes)
    {
        return (CalculateHours(HoursToMinutes(hours, minutes)), CalculteMinutes(HoursToMinutes(hours, minutes))
    }
    
    public override string ToString() => $"{Hours:00}:{Minutes:00}";
//    public bool Equals(Clock other) => Hours.Equals(other.Hours) && Minutes.Equals(other.Minutes);
}