using System;
using System.Collections.Generic;

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
    public Allergies(int mask)
    {
        this.mask = (Allergen)mask;
    }

    public bool IsAllergicTo(Allergen allergen)
    {
        return ((int)allergen & (int)mask) == (int)allergen;
    }

    public Allergen[] List()
    {
        var list = new List<Allergen>();
        foreach (var a in Enum.GetValues(typeof(Allergen)))
        {
            if (((int) a & (int) mask) == (int) a)
            {
                list.Add((Allergen)a);
            }
        }

        return list.ToArray();
    }
}