using System;
using System.Collections.Generic;
using System.Linq;

public class HighScores
{
    public readonly IEnumerable<int> scores;
    public HighScores(IEnumerable<int> scores) => this.scores = scores;

    public IEnumerable<int> Scores() => scores;

    public int Latest() => scores.Last();

    public int PersonalBest() => scores.Max();

    public IEnumerable<int> PersonalTopThree() => scores.OrderByDescending(i => i).Take(3);
}