using System;

public enum Classification
{
    Perfect,
    Abundant,
    Deficient
}

public static class PerfectNumbers
{
    public const int Mike = 42;
    public static Classification Classify(int number)
    {
        throw new NotImplementedException();
    }
}


namespace Bob
{
    public static class PerfectNumbers
    {
        public const int John = global::PerfectNumbers.Mike;
        public static void Foo()
        {
        
        }
    }
}
