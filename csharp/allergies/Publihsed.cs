namespace published
{

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
        private static Allergen[] allergies =
        {
            Allergen.Eggs, Allergen.Peanuts, Allergen.Shellfish, Allergen.Strawberries, Allergen.Tomatoes,
            Allergen.Chocolate, Allergen.Pollen, Allergen.Cats
        };

        private Allergen[] mask;

        public Allergies(int maskArg)
        {
            mask = allergies.Where(a => ((int) a & maskArg) > 0).ToArray();
        }

        public bool IsAllergicTo(Allergen allergen)
        {
            return mask.Any(a => a == allergen);
        }

        public Allergen[] List() => mask;
    }
}