using System;
using System.Collections.Generic;
using System.Linq;

public static class ParallelLetterFrequency
{
    public static Dictionary<char, int> Calculate(IEnumerable<string> texts)
    {
        // slower
        return texts
            .AsParallel()
            .Select(t => t.ToLower())
            .SelectMany(t => t, (t, c) => c)
            .Where(Char.IsLetter)
            .OrderBy(_ => _)
            .GroupBy(c => c)
            .ToDictionary(g => g.Key, g => g.Count());
    }
    public static Dictionary<char, int> CalculateSlower(IEnumerable<string> texts)
    {
        // both solutions here are slower as PARALLEL for the test cases
        // but faster for larger data sets
        // this solution is restricted to ascii 8 text
        var combinedText = texts.SelectMany(t => t.ToLower());
        var alphabet = Enumerable.Range(0, 255)
            .Select(c => (char)c)
            .Where(Char.IsLetter)
            .Intersect(combinedText);
        return alphabet
            .AsParallel()
            .SelectMany(c => combinedText
                    .Where(Char.IsLetter)
                , (c, x) => (c, c == x ? 1 : 0))
            .Where(p => p.Item2 != 0)
            .BuildDictionary(alphabet);
    }
    
}

internal static class Exts
{
    public static Dictionary<char, int> BuildDictionary(this IEnumerable<(char, int)> source,
        IEnumerable<char> alphabet)
    {
        var dict = alphabet.ToDictionary(a => a, a => 0);
        foreach (var p in source)
        {
            dict[p.Item1]++;
        }

        return dict;
    }
}

namespace MyTests
{
    using Xunit;
    public class MyTests
    {
        [Fact(Skip = "")]
        public void CalculateTest()
        {
            var input = Enumerable.Repeat(new[] { OdeAnDieFreude, Wilhelmus, StarSpangledBanner }, 10_000).SelectMany(t => t);
            var actual = ParallelLetterFrequency.Calculate(input);
            Assert.Equal(490000, actual['a']);
            Assert.Equal(560000, actual['t']);
            Assert.Equal(20000, actual['ü']);
        }
        [Fact(Skip = "")]
        public void CalculateSlowerTest()
        {
            var input = Enumerable.Repeat(new[] { OdeAnDieFreude, Wilhelmus, StarSpangledBanner }, 10_000).SelectMany(t => t);
            var actual = ParallelLetterFrequency.CalculateSlower(input);
            Assert.Equal(490000, actual['a']);
            Assert.Equal(560000, actual['t']);
            Assert.Equal(20000, actual['ü']);
        }
        private const string OdeAnDieFreude =
            "Freude schöner Götterfunken\n" +
            "Tochter aus Elysium,\n" +
            "Wir betreten feuertrunken,\n" +
            "Himmlische, dein Heiligtum!\n" +
            "Deine Zauber binden wieder\n" +
            "Was die Mode streng geteilt;\n" +
            "Alle Menschen werden Brüder,\n" +
            "Wo dein sanfter Flügel weilt.";

        // Dutch national anthem
        private const string Wilhelmus =
            "Wilhelmus van Nassouwe\n" +
            "ben ik, van Duitsen bloed,\n" +
            "den vaderland getrouwe\n" +
            "blijf ik tot in den dood.\n" +
            "Een Prinse van Oranje\n" +
            "ben ik, vrij, onverveerd,\n" +
            "den Koning van Hispanje\n" +
            "heb ik altijd geëerd.";

        // American national anthem
        private const string StarSpangledBanner =
            "O say can you see by the dawn's early light,\n" +
            "What so proudly we hailed at the twilight's last gleaming,\n" +
            "Whose broad stripes and bright stars through the perilous fight,\n" +
            "O'er the ramparts we watched, were so gallantly streaming?\n" +
            "And the rockets' red glare, the bombs bursting in air,\n" +
            "Gave proof through the night that our flag was still there;\n" +
            "O say does that star-spangled banner yet wave,\n" +
            "O'er the land of the free and the home of the brave?\n";
    }
}