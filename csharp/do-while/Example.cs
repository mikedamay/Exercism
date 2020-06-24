
using System.Collections.Generic;

public enum Badge
{
    Silver,
    Gold,
    Platinum,
    Paladium,
    Bronze
}

public class CourseResults
{
    public void AddBadges(Badge[] badges)
    {
        var badgeBag = new HashSet<Badge>();
        Badge badge = Badge.Bronze;
        do
        {
            Badge badge;
        } while (badge != Badge.Paladium);
    }
}