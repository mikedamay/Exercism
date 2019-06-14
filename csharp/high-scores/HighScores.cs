using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;

public class HighScores
{
    private readonly List<int> list;
    public HighScores(IEnumerable<int> list) => this.list = list.ToList();

    public IList<int> Scores() => new ReadOnlyCollection<int>(list);

    public int Latest() => list.Last();

    public int PersonalBest() => list.Max();

    public List<int> PersonalTopThree() => list.OrderByDescending(_ => _).Take(3).ToList();

}