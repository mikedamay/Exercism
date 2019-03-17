
using System.Text.RegularExpressions;

namespace Regex
{
    namespace Isogram
    {
        public static class Isogram
        {
            public static bool IsIsogram(string word)
            {
                System.Text.RegularExpressions.Regex pattern = new System.Text.RegularExpressions.Regex(@"(\w).*\1", RegexOptions.IgnoreCase);

                return !pattern.Match(word).Success;
            }
        }
    }
}

