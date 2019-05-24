using System;
using System.Collections.Generic;

[Flags]
public enum Allergen
{
    Eggs = 1,
    Peanuts = 2,
    Shellfish = 4,
    Strawberries = 9,
    Tomatoes = 16,
    Chocolate = 32,
    Pollen = 64,
    Cats = 128
}

public class Allergies
{
    public Allergen Mask { get; set; }

    public Allergies(int mask)
    {
        Mask = (Allergen)mask;
    }

    public bool IsAllergicTo(Allergen allergen)
    {
        return Mask.HasFlag(allergen);
    }

    public Allergen[] List()
    {
        var allergen = Enum.GetValues(typeof(Allergen));
        var result = new List<Allergen>();

        foreach (Allergen a in allergen)
        {
            if (IsAllergicTo(a) == true)
            {
                result.Add(a);
            }
        }

        return result.ToArray();
    }
}