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
        private const int POUNDS_PER_STONE = 14;
        private const float OUNCES_PER_POUND = 16f;
        private float weight;

        public Units Units { get; set; } = Units.Kilograms;
        public float Weight
        {
            get { return weight *  Reduction / 100; }
            set
            {
                if (value < 0)
                {
                    throw new ArgumentException("weight cannot be negative");
                }

                weight = value;
            }
        }

        public BritishWeight BritishWeight
        {
            get
            {
                float adjustedWeight = weight * Reduction / 100;
                float weightInPounds = Units == Units.Kilograms ? adjustedWeight * POUNDS_PER_KILOGRAM : adjustedWeight;
                return new BritishWeight(
                  (int)weightInPounds / POUNDS_PER_STONE
                  , (int)Math.Floor(weightInPounds)
                  , OUNCES_PER_POUND * (weightInPounds - (int)weightInPounds) );
            }
        }
        public float Reduction { set; private get; }
    }

    public struct BritishWeight
    {
        public BritishWeight(int stones, int pounds, float ounces)
        {
            Stones = stones;
            Pounds = pounds;
            Ounces = ounces;
        }

        public int Stones { get; }
        public int Pounds { get; }
        public float Ounces { get; }
    }
    
}
