using System.Text.RegularExpressions;

namespace Isogram
{
public static class Isogram
{
    public static bool IsIsogram(string word)
    {
        Regex pattern = new Regex(@"(\w).*\1", RegexOptions.IgnoreCase);

        return !pattern.Match(word).Success;
    }
}}