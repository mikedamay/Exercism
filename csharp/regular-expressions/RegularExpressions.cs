using System.Text.RegularExpressions;

public class LogParser
{
    public bool IsMatch(string text, string searchArg)
    {
        string[] matches = Regex.Matches(text, searchArg);
        return matches.Length > 0;
    }
}
