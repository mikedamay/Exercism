using System;

public class Clock
{
    private readonly int hours;
    private readonly int minutes;
    public Clock(int hours, int minutes)
    {
        (this.hours, this.minutes) = AdjustClock(hours, minutes, 0);
    }

    private Clock((int hours, int minutes) time) => (hours, minutes) = time;

    public Clock Add(int minutesToAdd)
        => new Clock(AdjustClock(hours, minutes, minutesToAdd));

    public Clock Subtract(int minutesToSubtract) => Add(-minutesToSubtract);

    public override string ToString()
    {
        var aa = $"{hours:D2}:{minutes:D2}";
        return aa;
    }

    private (int hoursOut, int minutesOut) AdjustClock(int hoursIn, int minutesIn, int adjustmenttMinutes)
    {
        const long FOR_EVER = 1_000_000_000_000 * 24 * 60;
        long baseTime = FOR_EVER + hoursIn * 60 + minutesIn + adjustmenttMinutes;
        return ((int)(baseTime % (24 * 60)/ 60), (int)(baseTime % 60));
    }

    private bool Equals(Clock other)
    {
        return hours == other.hours && minutes == other.minutes;
    }

    public override bool Equals(object obj)
    {
        if (ReferenceEquals(null, obj)) return false;
        if (ReferenceEquals(this, obj)) return true;
        if (obj.GetType() != this.GetType()) return false;
        return Equals((Clock) obj);
    }

    public override int GetHashCode()
    {
        unchecked
        {
            return (hours * 397) ^ minutes;
        }
    }
}