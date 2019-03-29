using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

public static class Dominoes
{
    private static bool result = false;
    public static bool CanChain(IEnumerable<(int, int)> dominoes)
    {
        if (!dominoes.Any())
            return true;
        ImmutableList<(int, int)> bag = ImmutableList<(int, int)>.Empty.AddRange(dominoes).RemoveAt(0);
        ImmutableList<(int, int)> hand = ImmutableList<(int, int)>.Empty.Add(dominoes.First());
        return PlayStone(bag, hand);
    }

    private static bool PlayStone(ImmutableList<(int, int)> bag, ImmutableList<(int, int)> hand)
    {
        if (bag.IsEmpty)
        {
            result = hand.First().Item1 == hand.Last().Item2;
            return result;
        }
        
        var candidates = GetCandidates(hand.Last(), bag);
        if (!candidates.Any())
        {
            result = false;
            return result;
        }
        foreach (var candidateStone in candidates)
        {
            if (candidateStone.Item2 == hand.Last().Item2)
            {
                if (PlayStone(bag.Remove(candidateStone), hand.Add((candidateStone.Item2, candidateStone.Item1))))
                    return true;

            }
            else
            {
                if (PlayStone(bag.Remove(candidateStone), hand.Add(candidateStone)))
                    return true;
            }
        }

        return false;
    }

    private static IEnumerable<(int, int)> GetCandidates((int, int) stoneToMatch, ImmutableList<(int, int)> bag)
    {
        return bag.Where(st => st.Item1 == stoneToMatch.Item2 || st.Item2 == stoneToMatch.Item2);
    }
}