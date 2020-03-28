using System;

public struct Clock
{
    private readonly int hours;
    private readonly int minutes;

    public Clock(int hours, int minutes)
    {
        var m = minutes;
        var s = Math.Sign(m);
        while (m >= 60 || m < 0)
        {
            hours += s;
            m -= s * 60;
        }
        this.minutes = m;

        var h = hours;
        s = Math.Sign(h);
        while (h >= 24 || h < 0)
        {
            h -= s * 24;
        }
        this.hours = h;
    }

    public Clock Add(int minutesToAdd) => new Clock(hours, minutes + minutesToAdd);


    public Clock Subtract(int minutesToSubtract) => new Clock(hours, minutes - minutesToSubtract);

    public override string ToString() => $"{hours.ToString().PadLeft(2, '0')}:{minutes.ToString().PadLeft(2, '0')}";
    
    public override int GetHashCode()
    {
        return HashCode.Combine(hours, minutes);
    }

}