using System;

namespace Example
{
    public enum Units
    {
        Kilograms
        ,Pounds
    }
    public class WeighingMachine
    {
        private const float POUNDS_PER_KILOGRAM = 2.20462f;
        private float inputWeight;

        public Units Units { get; set; } = Units.Kilograms;
        public float InputWeight
        {
            get { return inputWeight; }
            set
            {
                if (value < 0)
                {
                    throw new ArgumentException("weight cannot be negative");
                }

                inputWeight = value;
            }
        }

        public float DisplayWeight
        {
            get { return ApplyVanityFactor(inputWeight); }
        }
        public BritishWeight BritishWeight
        {
            get
            {
                return new BritishWeight(WeightInPounds(ApplyVanityFactor(inputWeight)));
            }
        }
        public float VanityFactor { set; private get; }
        private float ApplyVanityFactor(float weight) => weight * (100 - VanityFactor) / 100;
        private float WeightInPounds(float weight) => Units == Units.Kilograms ? weight * POUNDS_PER_KILOGRAM : weight;
    }

    public class BritishWeight
    {
        private const int POUNDS_PER_STONE = 14;
        private const float OUNCES_PER_POUND = 16f;

        public BritishWeight(float weightInPounds)
        {
            Stones = (int)weightInPounds / POUNDS_PER_STONE;
            Pounds = (int)Math.Floor(weightInPounds) - ((int)weightInPounds / POUNDS_PER_STONE) * POUNDS_PER_STONE;
            Ounces = (int)(OUNCES_PER_POUND * (weightInPounds - (int)weightInPounds));
        }


        public int Stones { get; }
        public int Pounds { get; }
        public int Ounces { get; }
    }
}
