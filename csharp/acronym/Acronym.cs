using System;
using System.Text;

public static class Acronym
{
    enum State
    {
        AfterSeparator,
        AfterStartOfWord,
        LetterEncountered
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
                    break;
            }
        }
        return sb.ToString();
    }
}