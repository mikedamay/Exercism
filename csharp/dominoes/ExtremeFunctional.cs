using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace ExtremeFunctional
{
    public class Dominoes
    {
        public static bool CanChain(IEnumerable<(int, int)> dominoes)
        {
            ImmutableList<(int, int)> bag() => ImmutableList<(int, int)>.Empty.AddRange(dominoes).RemoveAt(0);
            ImmutableList<(int, int)> hand() => ImmutableList<(int, int)>.Empty.Add(dominoes.First());
            return !dominoes.Any() ? true : PlayStone(bag(), hand());
        }

        private static bool PlayStone(ImmutableList<(int, int)> bag, ImmutableList<(int, int)> hand) =>
            bag.IsEmpty ? hand.First().Item1 == hand.Last().Item2 :
                GetCandidates(hand.Last(), bag).Any() 
                && HandleCandidate(ImmutableList<(int, int)>.Empty
                    .AddRange(GetCandidates(hand.Last(), bag)), bag, hand);

        private static bool HandleCandidate(ImmutableList<(int, int)> candidates
            , ImmutableList<(int, int)> bag, ImmutableList<(int, int)> hand)
        {
            (int, int) candidateStone() => candidates.First();
            (int, int) reverseStone() 
                => candidateStone().Item2 == hand.Last().Item2 ? (candidateStone().Item2, candidateStone().Item1) :
                    candidateStone();
        
            return candidates.Any() && (PlayStone(bag.Remove(candidateStone())
                                            , hand.Add(reverseStone())) || HandleCandidate(candidates.RemoveAt(0), bag, hand));
        }

        private static IEnumerable<(int, int)> GetCandidates((int, int) stoneToMatch, ImmutableList<(int, int)> bag)
        {
            return bag.Where(st => st.Item1 == stoneToMatch.Item2 || st.Item2 == stoneToMatch.Item2);
        }
        
    }
}