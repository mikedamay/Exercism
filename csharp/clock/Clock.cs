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
/*
A marker interface is one that expects to be detected at runtime and that callers will change their internal behaviour depending on the presence or absence of the interface.  Tools and runtime can detect the interface and validate instances or use them in a particular way.

[https://stackoverflow.com/questions/1023068/what-is-the-purpose-of-a-marker-interface](https://stackoverflow.com/questions/1023068/what-is-the-purpose-of-a-marker-interface)

In our case simply remove ` : IEquatable<Clock>` from the class declaration and you will see it makes no difference at all.  Note,  the  only change to make is to include the text `: IEquatable<Clock>` in the declaration or exclude it.  Both Equals methods remain in the definitition which ever way the declaration is expressed.

So, what's it for?  If you had a list of clocks and called Contains on them then the list's behaviour would vary depending on whether IEquatable is part of the declaration or not.  If IEquatable is declared then the List.Contains method will call Clock.Equals(Clock other) directly.  If IEquatable is not part of the declaration then List.Contains will call Clock.Equals(object obj) and thereby Clock.Equals(Clock other) indirectly.

So, what's the point?  The ONLY point of including `IEquatable<Clock>` in the declaration is to allow collections like `List<Clock>` to skip the unncessary call via `Clock.Equals(object obj)`.

*/

/*
The cardinal rule is that if Equals() returns true for two instances then GetHashCode() must return the same value for each of them.

However if two instances return the same value from GetHashCode() there is no correspoding guarantee that Equals() will return true.

It addresses the case where hashcodes are used to place instances in a "bucket" (in a hashset of dictionary) where all items in the bucket have the same hashcode.  A search routine can quickly find the bucket using the hashcode and then find the exact match within the bucket by then testing for equality.

You will doubtless be intested in what the significance of 397 is and why we XOR the values in `(Hours * 397) ^ Minute`.  Sorry I can't help here beyond saying this is obviously some way of evenly distributing values.  Hardly the soundest foundation on which to base your code.  If you come across a good explanation of what the variables are in the mechanism I should be interested. 
*/