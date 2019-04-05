using System;

public static class CollatzConjecture
{
    public static int Steps(int number)
    {
        int CountStep(int num)
        {
            if (num == 1)
            {
                return 0;
            }
            else
            {
                return 1 + CountStep(num % 2 == 0 ? num / 2 : num * 3 + 1);
            }
        }

        return number <= 0 ? throw new ArgumentException() : CountStep(number);
    }
}