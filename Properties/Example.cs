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
        private float weight;

        public Units Units { get; set; } = Units.Kilograms;
        public float Weight
        {
            get { return weight; }
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
                float weightInPounds = Units == Units.Kilograms ? weight * POUNDS_PER_KILOGRAM : weight;
                return new BritishWeight((int)weightInPounds / 14
                  , (int)Math.Floor(weightInPounds)
                  , 16f * (weightInPounds - (int)weightInPounds) );
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
