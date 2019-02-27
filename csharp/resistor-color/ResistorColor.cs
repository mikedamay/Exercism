using System;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using System.Net.Sockets;

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

As an alternative you could have used en Enum of colors.  I think the choice depends on how the values will be used in the program which is not obvious from the exercise notes.

*/

/*
The array is exposed without cloning.
*/

/*
The color array could be cloned as a defensive measure.
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