using System;

public static class Isogram
{
    public static bool IsIsogram(string word)
    {
        string loweredWord = word.ToLower();
        int[] count = new int[256];
        foreach(char c in word)
        {
            if (++count[c] > 1)
                return false;
        }
        return true;
    }
}