using System;
using System.Linq;

public static class RotationalCipher
{
    private const string codeLine = "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz";

    public static string Rotate(string text, int shiftKey)
    {
        var chars = text
            .Select(c => new {ch = Char.ToLower(c), upper = Char.IsUpper(c)})
            .Select(p => new {ch = Char.IsLower(p.ch) ? codeLine[p.ch - 'a' + shiftKey] : p.ch, 
                upper = p.upper})
            .Select(p => p.upper ? Char.ToUpper(p.ch) : p.ch).ToArray();
        
        return new string(chars);
    }
}

/*
Review Points:

That is a cool solution

Discussion Points:

If you're interested in discussing a more functional/LINQ-based solution then look at the community solutions and/or I would be happy to participate.
*/
/*
It seems to be me that is clueless - my suggestions were not practical.

On the subject of readability, I take your point.  To an OO dinosaur like me LINQ/functional code can be difficult to read.  However it does have other attributes that compensate (such as compile time safety - i.e. if it compiles the chances are it will behave correctly or at least much more so than OO code).  You have to get used to it.

If you are looking to use C# commercially or on an established OSS project than you need to be aware of the increasing fashion for a functional style in C#.  I don't think we'll know the extent to which the functional approach will become pervasive until we see some robust counter-arguments in favour of the OO style.

Anyway, to return to my cluelessness: as you say a `Where` is no use.  Let's try to move your solution to a LINQy one - as you say pretty much a rewrite but it might be illuminating.

Simplify some of the constructs:
* There is no need for Alphabet to be a `char[]`.  A string has all the functionality you need such as `IndexOf()`.
* Your current code would not handle non-English-alphabet letters correctly so a simple test is to pass through anything where the index returned is -1.
* Modify `GetShift` to `GetShifted` which returns the result character rather than the shift.
* Turn as much of `GetShifted` into expressions as possible.
We have:
```
public static class RotationalCipher
{
    private static readonly string Alphabet =
        "abcdefghijklmnopqrstuvwyxz";

    public static string Rotate(string text, int shiftKey)
    {
        return string.Concat(text
            .Select(x => (char) ( GetShifted(shiftKey, x))));
    }

    private static char GetShifted(
        int shiftKey,
        char c)
    {
        if (Alphabet.IndexOf(char.ToLower(c)) == -1)
        {
            return c;
        }

        var charIndex = Alphabet.IndexOf(char.ToLower(c));

        if (charIndex == -1)
            return c;

        return shiftKey + charIndex < Alphabet.Length 
                ? (char)(c + shiftKey) 
                : (char)(c + shiftKey - Alphabet.Length);
    }
}
```
This is no more LINQy than your solution but it is easier to transform into LINQ simply by virtue of being simpler.  The condition statements we can turn into ternary expressions that can sit inside `Select`s.  One problem is what to do with the `charIndex` variable.  Variables do not suit the functional style.

The solution is to create an object which includes both the item being processed by LINQ (in this case each char of `text` together with temporary information about it (in this case `chaarIndex`.

The result is:
```
public static class RotationalCipher
{
    private static readonly string Alphabet =
        "abcdefghijklmnopqrstuvwyxz";

    public static string Rotate(string text, int shiftKey)
    {
        return string.Concat(text
            .Select(x => new {@char = x, charIndex = Alphabet.IndexOf(char.ToLower(x))})
            .Select(p => p.charIndex == -1 ? p.@char : shiftKey + p.charIndex < Alphabet.Length 
                ? (char)(p.@char + shiftKey) 
                : (char)(p.@char + shiftKey - Alphabet.Length)));
    }

}
```
If you look at my community solution you will see that I gave a couple of tweaks to the algorithm that arguably make things simpler.

Sorry, for the earlier sloppy mentoring.
*/

/*
Review Points:

Good solution

Discussion Points:

It is debatable whether `Char.IsLetter()` is the appropriate test here.  It would cause non-english-alphabet letters to be shifted with undetermined results.
*/