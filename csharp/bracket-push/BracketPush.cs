using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Data;
using System.Linq;

public static class BracketPush
{
    private const char NO_BRACKET = '\0';
    private static IReadOnlyDictionary<char, char> closingBracketLookup
      = new Dictionary<char, char>
      {
          {'(', ')'},
          {'[', ']'},
          {'{', '}'},
      };
    public static bool IsPaired(string input)
    {
        int idx = 0;
        bool MatchBracket(ref int ii, char expectedClosingBracket)
        {
            // Debug.Assert( map.ContainsKey(expectedBracket);
            while (ii < input.Length)
            {
                var jj = ii;
                if (closingBracketLookup.Values.Any(c => c == input[jj]))
                {
                    ii++;
                    return input[jj] == expectedClosingBracket;
                }

                if (closingBracketLookup.ContainsKey(input[ii]))
                {
                    var openingBracket = input[ii++];
                    var rtn = MatchBracket(ref ii, closingBracketLookup[openingBracket]);
                    if (!rtn)
                    {
                        return false;
                    }
                }
                else
                    ii++;
            }

            return expectedClosingBracket == NO_BRACKET;
        }

        return MatchBracket(ref idx, NO_BRACKET);
    }
}
