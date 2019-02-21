using System;
using System.Collections.Generic;
using System.Linq;

public class HighScores
{
    private readonly List<int> list;
    public HighScores(List<int> list)
    {
        this.list = list;
    }

    public List<int> Scores()
    {
        return list;
    }

    public int Latest()
    {
        return list.Last();
    }

    public int PersonalBest()
    {
        return list.Max();
    }

    public List<int> PersonalTop()
    {
        return list.OrderByDescending(n => n).Take(3).ToList();
    }

    public string Report()
    {
        var best = $"Your latest score was {Latest()}. That's your personal best!";
        var lesser = $"Your latest score was {Latest()}. That's {PersonalBest() - Latest()} short of your personal best!";
        return Latest() == PersonalBest() ? best : lesser;
    }
}