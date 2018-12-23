using System;
using System.Collections.Generic;
using System.Linq;

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
        return ((int)mask & (int)allergen) > 0;
    }

    public Allergen[] List()
    {
        var list = new List<Allergen>();
        for (int ii = 0; ii <= 7; ii++)
        {
            if (((int)mask & (1 << ii)) > 0)
            {
                list.Add((Allergen)(1<<ii));
            }
        }

        return list.ToArray();
    }
}