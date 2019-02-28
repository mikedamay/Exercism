using System;
using System.Text;

public static class Standard
{
    public static string Abbreviate(string phrase)
    {
        int[] aa = { 1, 2};
        /* assumes that ' will never occur before an abbreviatable word */
        bool IsWordSeparator(char c) => !(char.IsLetter(c) || c == '\'');
        var sb = new StringBuilder();
        if (char.IsLetter(phrase[0]))
            sb.Append(char.ToUpper(phrase[0]));
        for (int ii = 1; ii < phrase.Length; ii++)
        {
            if (char.IsLetter(phrase[ii]) && IsWordSeparator(phrase[ii - 1]))
                sb.Append(char.ToUpper(phrase[ii]));
        }

        return sb.ToString();
    }
}