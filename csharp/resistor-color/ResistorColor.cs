using System;

public static class ResistorColor
{
    enum Color
    {
        black,
        brown,
        red,
        orange,
        yellow,
        green,
        blue,
        violet,
        grey,
        white
        
    }
    public static int ColorCode(string color)
    {
        return (int)Enum.Parse(typeof(Color), color);
    }

    public static string[] Colors()
    {
        return Enum.GetNames(typeof(Color));        
    }
}

/*
Good solution.

I like the `ReadOnlyDictionary` with initializers.

As an alternative you could have used en Enum of colors.
I think the choice depends on how the values will be 
used in the program which is not obvious from the exercise notes.

*/

/*
consider cloning the array on return from `Colors()` as
a defensive measure.  This will prevent a user of the class from
accidentally overwriting the values.
*/

/*
You could make the dictionary read only in addition to the dictionary field.
Making the field read only simply makes it clear that a new dictionary will 
not be assigned to this field and that the field could never be null whereas 
making the dictionary itself read only will signal that items in the collection 
will not be added to, deleted or replaced.


 */

/*
An effective solution.

Have a look at `Enum.Parse()` and `Enum.GetValues()` and decide whether you would prefer a version using these member methods rather than LINQ.

I would expect LINQ to be slower but you can't always tell.
*/

/*
Effective solution

Rebuilding the color array for each call to `ColorCode()` seems a little excessive.  What do you think?
*/
/*
One interesting thing to be aware of here is that through the array
of colors you are exposing the internal data of the class.
A foolish maintainer might call `Colors()` and then modify 
the returned array causing problems for later calls on the class.

Think about how you might address this problem (and to what extent
 / under what circumstances) such "defensive coding" is appropriate.
*/
/*
Your confusion over the return value from IndexOf() is utterly justified.  What a mediocre piece of documentation that particular MSDN article is!  Incidentally make sure you use the latest documentation at [docs.microsoft.com](http://docs.microsoft.com).

Actually in the docs.microsoft.com version of the documentation the text you quote is from the overload that takes an `Array` rather than a conventional array.  Other overloads of `IndexOf()` do mention -1.  But none the less poor guidance unless the reader happens to be looking for bear traps.

The short answer is that in 99.9999% of cases the return values in a C# program is -1 and the following code would be expected in your solution:
```
var cc = Array.IndexOf(colorCodeArr, color);
return cc != -1 ? cc ? thrown new ArgumentException();
```
So why can't the docs just say that?

There is a different sort of array (an instance of the class `System.Array`) which allows the lower bound of the array to be set by the caller see [http://csharphelper.com/blog/2015/05/make-arrays-with-non-zero-lower-bounds-in-c/](http://csharphelper.com/blog/2015/05/make-arrays-with-non-zero-lower-bounds-in-c/).  It is unfortunate the example in the article is for a 2 dimensional array.

`Array`'s static methods have to operate on both traditional arrays (e.g. `new int[10]`) and instances of `Array`.

You will see from the article above that if you create an array say with the bounds 2000 to 2009 (`Array.CreateInstance(typeof(string), new int[] {10}, new int[] {2000})`) then a call to `arr.GetValue(0)` will cause an out-of-bounds exception where as a call to say `arr.GetValue(2003)` will return 0 (or whatever is subsequently stored there.  Presumable `IndexOf` returns 1999.  - I've certainly never used any of this stuff.

As far as I am aware there is no easy way to convert between instances of `Array` and conventional arrays.

Why all this nonsense?

.NET libraries support the CLR (Common Language Runtime) rather than a particular language.  One CLR language, VB.NET, had to keep faith with its ancestor Visual Basic.  In Visual Basic these non-zero bounded arrays were first class citizens and had the same form as conventional arrays.
*/

/*
Review Points:

This is a good solution

`ColorCode()` causes the color array to be instantiated each time it is called.  This is not optimally performant.

A bit of defensive programming would be to clone the color array before returning it from `Colors()` to prevent maintainers from inadvertently modifying the `ResistorColor` data.

Discussion Points:

Arguably the most maintainable solution would be a dictionary mapping string color names to explicit enums. This would apply if both color names and color values are mapped outside of this code, for example in a design document or a spec. Implicit mappings are less maintainable. As a bonus it's easier to mentally map the dictionary to the spec file.
*/

/*
Review Points:

Good solution.

You could make the color mapping a `ReadOnlyDictionary` as a strong guide to maintainers.

Discussion Points:

Something else to consider is the user of enums.  It all depends on how the data structures will be used in the wider program.
*/

/*
You usually catch this sort of thing in beginner tutorials or language updates.

You might reasonably expect to find a language spec but this does not seem to be maintained.  The latest is a draft for C# 6 followed by language updates for 7.0, 7.1, 7.2 and 7.3 (current).

You can find a lot of this at https://docs.microsoft.com/en-gb/dotnet/csharp/language-reference/
*/
/*
Be careful - others may consider some of my views unorthodox.

The issue for me is not whether identifiers can
be abbreviated or not but what code provides the 
best experience for a maintainer (subject to meeting 
non-functional requirements such as performance,
 operational robustness and resilience).

The experience for maintainers relates to: 
code-readabilty, how easy is it to reason 
about the code and whether or not there 
are gotchas (defensive coding)
*/
/*
I like this

I usually comment on the use of `Colors()` in `ColorCode()`
being non-performant but the ubiquitous use of expression
bodied members makes the case "for" compelling.  As long
as you understand that the color array will be instantiated
each time `Colors()` is called.  There is no concept
of pure functions in C#.
*/

/*
The API for `ReadOnlyDictionary` is not straight forward.  I think the developers were feeling a bit lazy that day.

The easiest approach and, arguably, the most appropriate here is to expose the dictionary as a `IReadOnlyDictionary` as follows:
```
static IReadOnlyDictionary<string, int> CodeConversion = new Dictionary<...> {...}
```

This is simple but it does allow users of the object to cast it back to a dictionary and circumvent your read only mechanism.

To avoid read only circumvention you can create a non-castable `ReadOnlyDictionary` as follows:
```
static Dictionary<string, int> CodeConversionWritable = new Dictionary<stirng, int> { {"black, 0}...};
static ReadOnlyDictionary<string, int> CodeConversion = new ReadOnlyDictionary(CodeConversionWriteable);
```
*/