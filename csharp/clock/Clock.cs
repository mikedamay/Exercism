public class Clock
{
    private readonly int timeInMinutes;
    public Clock(int hours, int minutes)
    {
        this.timeInMinutes = AdjustClock(hours * 60 + minutes, 0);
    }

    private Clock(int timeInMinutes) => this.timeInMinutes = timeInMinutes;

    public int Hours => timeInMinutes / 60;
    public int Minutes => timeInMinutes % 60;
    
    public Clock Add(int minutesToAdd)
        => new Clock(AdjustClock(timeInMinutes, minutesToAdd));

    public Clock Subtract(int minutesToSubtract) => Add(-minutesToSubtract);

    public override string ToString()
    {
        return $"{Hours:D2}:{Minutes:D2}";
    }

    private int AdjustClock(int minutesIn, int adjustmenttMinutes)
    {
        return (24 * 60 + ( minutesIn + adjustmenttMinutes) % (24 * 60)) % (24 * 60);
    }

    private bool Equals(Clock other)
    {
        return timeInMinutes == other.timeInMinutes;
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
        return timeInMinutes;
    }
}