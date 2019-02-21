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

namespace MyTests 
{    
    using System.Collections.Generic;
    using Xunit;

    public class MyTests
    {
        [Fact(Skip = "")]
        public void Personal_best()
        {
            Assert.Throws<InvalidOperationException>(
                () =>
                {
                    var sut = new HighScores(new List<int> { }).PersonalBest();
                });
        }
    }
    
}
/*
This is an effective solution.

A couple of comments:

in `PersonalBest()` initialising `TopScores` with a brand new list is a waste of effort as you immediately overwrite it with the list returned from `PeresonalTop()`.

`PersonalTop()[0]` is a valid expression if you wanted to shorten the implementation.

You've used `Enumerable.Last()` which is a LINQ method.  Also available are:
```
Enumerable.Max()
Enumerable.OrderByDescending()
Enumerable.Take()
Enumerable.ToList()
```
This exercise is useful as a gentle introduction to LINQ.  In particular use of LINQ methods avoids mutating the list.  Mutability is very much out of fashion at present.

Make whatever changes you want, ask questions, argue as you see fit.