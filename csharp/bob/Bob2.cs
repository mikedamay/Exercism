using System;
using System.Text.RegularExpressions;

public static class Bobx
{
    public static string Response(string statement)
    {
        bool question = false;
        bool yelling = false;

        statement = RemoveSpace(statement);

        if (statement.Length == 0)
            return "Fine. Be that way!";

        if (statement.IndexOf("?")==statement.Length-1)
            question = true;
        if (IsYelling(statement))
            yelling = true;

        if (question && yelling)
            return "Calm down, I know what I'm doing!";
        if (question)
            return "Sure.";
        if (yelling)
            return "Whoa, chill out!";
        
        return "Whatever.";
    }
    private static bool IsYelling(string statement)
    {
        if (IsDigitsOnly(statement))
            return false;

        string upperCaseStatement = statement.ToUpper();
        if (statement.Equals(upperCaseStatement))
            return true;
       
        return false;
    }
    private static bool IsDigitsOnly(string str)
    {
        foreach (char c in str)
        {
            if (char.IsLetter(c))
                return false;
        }

        return true;
    }
    private static string RemoveSpace(string str)
    {
//        return str.Replace("\t", string.Empty).Replace(" ", string.Empty).Replace("\n", string.Empty)
//            .Replace("\r", string.Empty);
        return Regex.Replace(str, @"\s+", string.Empty);
    }
}