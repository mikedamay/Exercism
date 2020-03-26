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
        private const decimal POUNDS_PER_KILOGRAM = 2.20462m;
        private decimal inputWeight;

        public Units Units { get; set; } = Units.Kilograms;
        public decimal InputWeight
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

        public decimal DisplayWeight
        {
            get { return ApplyVanityFactor(inputWeight); }
        }
        public BritishWeight BritishWeight
        {
            get
            {
                return new BritishWeight(WeightInPounds(DisplayWeight));
            }
        }
        public decimal VanityFactor { set; private get; }
        private decimal ApplyVanityFactor(decimal weight) => weight * (100 - VanityFactor) / 100;
        private decimal WeightInPounds(decimal weight) => Units == Units.Kilograms ? weight * POUNDS_PER_KILOGRAM : weight;
    }

    public class BritishWeight
    {
        private const int POUNDS_PER_STONE = 14;
        private const decimal OUNCES_PER_POUND = 16m;

        public BritishWeight(decimal displayWeightInPounds)
        {
            Stones = (int)displayWeightInPounds / POUNDS_PER_STONE;
            Pounds = (int)displayWeightInPounds % POUNDS_PER_STONE;
            Ounces = (OUNCES_PER_POUND * (displayWeightInPounds - (int)displayWeightInPounds));
        }
        
        public int Stones { get; }
        public int Pounds { get; }
        public decimal Ounces { get; }
    }
}
