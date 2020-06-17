using System;
using System.Collections.Generic;

public class Clock : IEquatable<Clock>
{
    private const int MinutesPerHour = 60;
    private const int MinutesPerDay = 24 * MinutesPerHour;

    private readonly int totalMinutes;

    private int Hours => totalMinutes / MinutesPerHour;
    private int Minutes => totalMinutes % MinutesPerHour;

    public Clock(int hours, int minutes) : this(hours * MinutesPerHour + minutes) { }
    public Clock(int minutes) => totalMinutes = (minutes % MinutesPerDay + MinutesPerDay) % MinutesPerDay;

    private static string ToPaddedString(int n, int pad = 2) => n.ToString($"D{pad}");

    public Clock Add(int minutesToAdd) => new Clock(totalMinutes + minutesToAdd);
    public Clock Subtract(int minutesToSubtract) => Add(-minutesToSubtract);

    public override string ToString() => $"{ToPaddedString(Hours)}:{ToPaddedString(Minutes)}";

    public override bool Equals(object obj) => obj is Clock clock && Equals(clock);
    public bool Equals(Clock other) => other?.totalMinutes == totalMinutes;
    public override int GetHashCode() => HashCode.Combine(totalMinutes);

    public static bool operator ==(Clock left, Clock right) => EqualityComparer<Clock>.Default.Equals(left, right);
    public static bool operator !=(Clock left, Clock right) => !(left == right);
}