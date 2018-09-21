using System;
using System.Linq;

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
    private Allergen mask;
    public Allergies(int maskArg)
    {
        mask = (Allergen)maskArg;
    }

    public bool IsAllergicTo(Allergen allergen)
    {
        return mask.HasFlag(allergen);
    }

    public Allergen[] List() => Enum.GetValues(typeof(Allergen)).Cast<Allergen>().Where(a => mask.HasFlag(a)).ToArray();
}