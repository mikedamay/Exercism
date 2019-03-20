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

Make whatever changes you want, ask questions, argue as you see fit and I will sign off.
*/

/*
Null has been described by its creator as the [billion dollar mistake](https://www.infoq.com/presentations/Null-References-The-Billion-Dollar-Mistake-Tony-Hoare).

One way to ensure that you will never see a NullReferenceException is to avoid setting anything to null in the first place.

In this exercise the decision to avoid `null` is a "no-brainer".  There was no downside to assigning the return value from `myScores.OrderByDescending` directly to the list variable.  In fact it made the code marginally more readable.

However you will encounter myriad situations where `null` is the correct approach (if the language, like C#, supports it).  On many occasions the cost or consequences of "newing" up a new instance are prohibitive.  Or, you may work with libraries where null is used as convenient out-of-band value.  In fact you may have to incorporate null as an out-of-band value in your own constructs.

The advice is to try to avoid null and be careful with it when you have to use it.

The next version of C#, v8, will be promoting this idea with language features.

> Should you not try to avoid the cost being a problem?

When you are developing an application (as opposed to a general purpose library) you are in a position to  weigh up the cost of  optimising for performance against readability / maintainability.  The conventional wisdom is to avoid optimising prematurely, wait until you can run a profiler against a fairly complete code base and allow the tool to guide you as to what code should be optimised.  Experience (including mine) is that, in the absence of the tool, much of the time you incorrectly diagnose which parts of the code need optimising.

Personally, I don't think this advice should be taken too far otherwise, as you say, fixing performance problems could be a real headache. If you know that some construct or pattern will be pervasive throughout your code base (for instance, adopting some helper library) then that is a good time to do some comparative testing on alternative implementations.

Finally, if you are developing a general purpose library then performance should be given greater consideration as you don't know how the library will be used.
*/

/*
Review Points:

Effective solution

Discussion Points:

If you prefer a solution with a more functional feel for `PersonalTop3()` then `OrderByDescending()` and `Take()` are available in `IEnumerable`
*/

/*
`Take()` works well with fewer items in the list than the number taken.
*/