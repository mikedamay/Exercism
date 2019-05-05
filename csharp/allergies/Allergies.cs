using System;
using System.ComponentModel.DataAnnotations;
using System.Linq;

namespace xx {
[Flags]
public enum Allergen
{
    Eggs = 1 << 0,
    Peanuts = 1 << 1,
    Shellfish = 1 << 2,
    Strawberries = 1 << 3,
    Tomatoes = 1 << 4,
    Chocolate = 1 << 5,
    Pollen = 1 << 6,
    Cats = 1 << 7
}

public class Allergies
{
    private readonly Allergen mask;
    public Allergies(int maskArg)
    {
        mask = (Allergen)maskArg;
    }

    public bool IsAllergicTo(Allergen allergen)
    {
        return mask.HasFlag(allergen);
    }

    public Allergen[] List() 
        => Enum.GetValues(typeof(Allergen))
            .Cast<Allergen>()
            .Where(a => mask.HasFlag(a))
            .ToArray();
    }

}
/*
Both `IsAllergicTo()` and `List()` can be reduced to fairly short methods.

As you suggest you can use the bitwise __and__ operator `&` to implement `IsAlergicTo()`.

for `List()` you can loop through all the possible allergens and again use the __and__ operator to decide whether to add them to the array.

There is a lot to learn from the Allergies exercise including fairly advanced LINQ and use of enum `[Flags]` but it is probably best to start with an implementation based on bitwise operators and build up from there.
*/

/*
`Enum.HasFlag` is slightly more readable in the `IsAllergicTo` implementation.
*/

/*
Review Points:

Good solution

Discussion Points:

Have a look in the community solutions for those that use the `[flags]` attribute on the enum and `Enum.HasFlag()`.  I will be happy to discuss.

A more gotcha-proof way of initializing the enum is with shifted integers, e.g.:
```
Shellfish = 1 << 2,
Strawberries = 1 << 3,
```

*/