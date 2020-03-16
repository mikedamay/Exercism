using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

public static class Change
{
    public static int[] FindFewestCoins(int[] coins, int target)
    {
        if (target == 0)
        {
            return new int[0];
        }
        else if (target < 0)
        {
            throw new ArgumentException();
        }
        else
        {
            (ImmutableList<int> coinsResult, int underPaid) 
              = FindFewestCoins(new List<int>(coins.Reverse()), target, ImmutableList<int>.Empty);
            if (underPaid != 0)
            {
                throw new ArgumentException();
            }
            else
            {
                return coinsResult.Reverse().ToArray();
            }
        }
        
        (ImmutableList<int> coins, int underPaid) FindFewestCoins(IEnumerable<int> denominations
            , int target
            , ImmutableList<int> coins)
        {
            (ImmutableList<int> coins, int underPaid) winner = (ImmutableList<int>.Empty, -1);
            var denoms = denominations.Where(d => d <= target);
            if (denoms.Count() == 0)
            {
                return (coins, target);
            }
            foreach (var denom in denoms.RemoveFactors(target))
            {
                (ImmutableList<int> coins, int underPaid) trial = (ImmutableList<int>.Empty, -1);
                if (target - denom == 0)
                {
                    coins = coins.Add(denom);
                    return (coins, 0);
                }
                else if (target - denom < 0)
                {
                    trial = FindFewestCoins(denoms.Skip(1), target, coins);
                }
                else
                {
                    trial = FindFewestCoins(denoms, target - denom, coins.Add(denom));
                }

                winner = FindWinner(winner, trial);
            }

            return winner;
        }
    }

    private static (ImmutableList<int> coins, int underPaid) FindWinner(
      (ImmutableList<int> coins, int underPaid) currentWinner, (ImmutableList<int> coins, int underPaid) trial)
    {
        if (trial.underPaid != 0)
        {        // attempt to build list failed
            return currentWinner;
        }
        else if (currentWinner.underPaid != 0)
        {
            return trial;
        }
        return currentWinner.coins.Count <= trial.coins.Count ? currentWinner : trial;
    }

    private static int GetCoinCount(ImmutableList<int> coins)
    {
        return coins.Sum();
    }

    private static IEnumerable<int> RemoveFactors(this IEnumerable<int> denomsArg, int target)
    {
        List<int> highers = denomsArg.ToList();
        List<int> lowers = denomsArg.ToList();
        for (int ii = 0; ii < highers.Count; ii++)
        {
            for (int jj = lowers.Count - 1; jj >= 0; jj--)
            {
                if (highers[ii] > lowers[jj] && highers[ii] % lowers[jj] == 0
                  &&  highers[ii] + lowers[jj] <= target)
                {
                    lowers.RemoveAt(jj);
                }
            }

        }

        foreach (int denom in lowers)
        {
            yield return denom;
        }
    }
}