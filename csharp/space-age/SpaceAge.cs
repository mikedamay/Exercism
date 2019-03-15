using System;

public class SpaceAge
{

    private const double EarthSeconds = 31557600;
    private const double MercurySeconds = EarthSeconds * 0.2408467;
    private const double VenusSeconds = EarthSeconds * 0.61519726;
    private const double MarsSeconds = EarthSeconds * 1.8808158;
    private const double JupiterSeconds = EarthSeconds * 11.862615;
    private const double SaturnSeconds = EarthSeconds * 29.447498;
    private const double UranusSeconds = EarthSeconds * 84.016846;
    private const double NeptuneSeconds = EarthSeconds * 164.79132;
    private readonly double seconds;
    public SpaceAge(long seconds)
    {
        this.seconds = seconds;
    }

    public double OnEarth()
    {
        return seconds / EarthSeconds;
    }

    public double OnMercury()
    {
        return seconds / MercurySeconds;
    }

    public double OnVenus()
    {
        return seconds / VenusSeconds;
    }

    public double OnMars()
    {
        return seconds / MarsSeconds;
    }

    public double OnJupiter()
    {
        return seconds / JupiterSeconds;
    }

    public double OnSaturn()
    {
        return seconds / SaturnSeconds;
    }

    public double OnUranus()
    {
        return seconds / UranusSeconds;
    }

    public double OnNeptune()
    {
        return seconds / NeptuneSeconds;
    }
}
/*
This solution is more complex than I would expect in a commercial situation.  
However I guess that this is because you are investigating the highways and 
byways of the language and ecosystem.  If not, let me know and we can discuss.
*/
/*
Minimum-Necessary

"we can go to a stopper" - this is very true but in spite of that agile methods favour doing the minimum necessary and refactoring as circumstances change.

Risks
* The risk of minimum-necessary is that a rewrite will be necessary and that is a risk the proponents, like me, of minimum-necessary are prepared to take.  

Costs
1. The cost of NOT doing minimum-necessary is that you constrain maintainers who may want to change the implementation of the class or modify it in some other way.  
2. In addition you make it harder for them to reason about the code as they will be considering the possibility of sub-classes. 

I see that this directly clashes with the open-close principle.  I must re-read Uncle Bob / Bertrand Meyer (who knew).  I don't think that it means that you can't change behaviour of a class to reflect a change in circumstances.

For me it's important that the code communicates the maximum amount about what it does and what else it might effect.  Code is generally written/modified a few times and read many more.

All this just because some people want to bring Pluto back as a planet.
*/

/*
Good solution

Good use of expression bodied members

If you make the dictionary a ReadOnlyDictionary then this will provide the clearest guidance to maintainers
*/  

/*
Review Points:

Good solution
 
Discussion Points:
 
Arguably it is helpful to gather the year values 
together in one place say as a bunch of constants 
or a dictionary of some sort.  This helps when 
reasoning about the validity of the values (as 
they are easy to compare) amongst other things.
The downside is that you have to make multiple
code changes when you add or remove a planet.
*/

/*
I find the const names slightly overwhelming.  
Given that the maintainer is aware of the context 
I think something more abbreviated might work.  Thoughts?
*/

