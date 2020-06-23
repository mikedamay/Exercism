using System.Text.RegularExpressions;

public class LogParser
{
    public bool IsMatch(string text, string searchArg)
    {
        var matches = Regex.Matches(text, searchArg);
        return matches.Count > 0;
    }
}
