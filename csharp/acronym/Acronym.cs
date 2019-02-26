using System;
using System.Text;

public static class Acronym
{
    enum State
    {
        AfterSeparator,
        AfterStartOfWord,
    }
    public static string Abbreviate(string phrase)
    {
        bool IsContinuationChar(char c) => Char.IsLetter(c) || c == '\'';
        bool IsInitialChar(char c) => Char.IsLetter(c);
        var sb = new StringBuilder();
        var state = State.AfterSeparator;
        foreach (var c in phrase)
        {
            switch (state)
            {
                case State.AfterSeparator:
                    if (IsInitialChar(c))
                    {
                        sb.Append(Char.ToUpper(c));
                        state = State.AfterStartOfWord;
                    }
                    // else still a separator
                    break;
                case State.AfterStartOfWord:
                    if (!IsContinuationChar(c))
                        state = State.AfterSeparator;
                    // else still part of the word
                    break;
            }
        }
        return sb.ToString();
    }
}

/*
2. For performance reasons, in almost all cases (and I'm sure it applies here) you should use a `StringBuilder` object to build up the output.

3. Also regarding performance use of strings with LINQ `Any()` rather than lists of chars would make a slight improvement (of the order of 5%).  However using string+intrinsic methods (`IndexOf()`) makes a 10x improvement compared to LINQ
*/