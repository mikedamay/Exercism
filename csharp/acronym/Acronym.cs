using System;
using System.Collections.Generic;

public static class Acronym
{
    public static string Abbreviate(string phrase)
    {
        string acronym = "";

        char[] delimiters = new char[] { ',', '-', ' ', '_' };
        string[] splitedWords = phrase.Split(delimiters,StringSplitOptions.RemoveEmptyEntries);

        for (int i = 0; i < splitedWords.Length; i++)
        {
            acronym += splitedWords[i][0];
        }

        return acronym.ToUpper();
    }
}