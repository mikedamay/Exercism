public static class BeerSong
{
    public static string Recite(int startBottles, int takeDown)
    {
        string verses = null;
        for (int ii = startBottles; ii > startBottles - takeDown; ii--)
        {
            var s = ii == 1 ? string.Empty : "s";
            var s_ = ii - 1 == 1 ? string.Empty : "s";
            var bottleCount = ii;
            var bottleCount2 = ii - 1 == 0 ? "no more" : (ii - 1).ToString();
            var one_or_it = ii == 1 ? "it" : "one";
            
            if (ii != startBottles)
                verses += "\n\n";
            if (ii == 0)
                verses +=
                    "No more bottles of beer on the wall, no more bottles of beer.\n" +
                    "Go to the store and buy some more, 99 bottles of beer on the wall.";
            else
                verses +=
                    $"{bottleCount} bottle{s} of beer on the wall, {bottleCount} bottle{s} of beer.\n" +
                    $"Take {one_or_it} down and pass it around, {bottleCount2} bottle{s_} of beer on the wall.";
        }

        return verses;
    }
}