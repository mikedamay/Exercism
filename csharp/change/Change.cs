﻿using System;
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
              = FindFewestCoins(new List<int>(coins), target, ImmutableList<int>.Empty);
            if (underPaid != 0)
            {
                throw new ArgumentException();
            }
            else
            {
                return coinsResult.Reverse().ToArray();
            }
        }
        
        (ImmutableList<int> coins, int underPaid) FindFewestCoins(List<int> denominations
            , int target
            , ImmutableList<int> coins)
        {
            (ImmutableList<int> coins, int underPaid) winner = (ImmutableList<int>.Empty, -1);
            if (denominations.Count == 0)
            {
                return (coins, target);
            }
            foreach (var denom in denominations)
            {
                (ImmutableList<int> coins, int underPaid) trial = (ImmutableList<int>.Empty, -1);
                if (target - denom == 0)
                {
                    coins = coins.Add(denom);
                    return (coins, 0);
                }
                else if (target - denom < 0)
                {
                    trial = FindFewestCoins(denominations.GetRange(0, denominations.Count - 1), target, coins);
                }
                else
                {
                    coins = coins.Add(denom);
                    trial = FindFewestCoins(denominations, target - denom, coins);
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
            
        return currentWinner.coins.Count <= trial.coins.Count ? currentWinner : trial;
    }

    private static int GetCoinCount(ImmutableList<int> coins)
    {
        return coins.Sum();
    }
}