public class Clock
{
    private readonly int timeInMinutes;
    
    public Clock(int hours, int minutes)
    {
        timeInMinutes = AdjustClock(hours * 60 + minutes, 0);
    }

    public int Hours => timeInMinutes / 60;
    public int Minutes => timeInMinutes % 60;
    
    public Clock Add(int minutesToAdd) => new Clock(Hours, Minutes + minutesToAdd);

    public Clock Subtract(int minutesToSubtract) => Add(-minutesToSubtract);

    public override string ToString()
    {
        return $"{Hours:D2}:{Minutes:D2}";
    }

    private int AdjustClock(int minutesIn, int adjustmentMinutes)
    {
        return (24 * 60 + ( minutesIn + adjustmentMinutes) % (24 * 60)) % (24 * 60);
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
/*
 1. You need the Equals(object) in order for your object to honour its contracts. In inheriting from object you assert that you can be passed an object for comparison so you had better handle it correctly.
 2. The first ReferenceEquals is null protection.
 3. The second ReferenceEquals is for performance
 4. GetHashCode() and the need for consistency with Equals are a bit more hazy. The hashcode is used, in particular, by dictionaries and sets and the bumping with a random number apparently spreads out the distribution in whatever tree structure they use. I suspect 5 moinutes of introspection or web search would reveal the importance of equality in this mix.
*/
