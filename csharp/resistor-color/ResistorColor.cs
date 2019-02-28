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

The short answer is that in 99.9999% of cases the return values in a C# program is -1 and the following code would be expected in your solution:
```
var cc = Array.IndexOf(colorCodeArr, color);
return cc != -1 ? cc ? thrown new ArgumentException();
```
So why can't the docs just say that?

There is a different sort of array (an instance of the class `System.Array`) which allows the lower bound of the array to be set by the caller see [http://csharphelper.com/blog/2015/05/make-arrays-with-non-zero-lower-bounds-in-c/](http://csharphelper.com/blog/2015/05/make-arrays-with-non-zero-lower-bounds-in-c/).  It is unfortunate the example in the article is for a 2 dimensional array.

`Array`'s static methods have to operate on both traditional arrays and instances of `Array`.

You will see from the article above that if you create an array say with the bounds 2000 to 2009 (`Array.CreateInstance(typeof(string), new int[] {10}, new int[] {2000})`) then a call to `arr.GetValue(0)` will cause an out-of-bounds exception where as a call to say `arr.GetValue(2003)` will return 0 (or whatever is subsequently stored there.  Presumable `IndexOf` returns 1999.  - I've certainly never used any of this stuff.

As far as I am aware there is no easy way to convert between instances of `Array` and conventional arrays.

Why all this nonsense?

.NET libraries support the CLR (Common Language Runtime) rather than a particular language.  One CLR language, VB.NET, had to keep faith with its ancestor Visual Basic.  In Visual Basic these non-zero bounded arrays were first class citizens and had the same form as conventional arrays.
*/