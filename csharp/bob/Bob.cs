using System;
using System.Linq;
using System.Text.RegularExpressions;

public static class Bob
{

    public static string Response(string statement)
    {
        (bool upperCasePresent, bool lowerCasePresent) Analyse(string str)
            => (
                str.Any(c => Char.IsUpper(c))
                , str.Any(c => Char.IsLower(c)));
        
        if (string.IsNullOrWhiteSpace(statement))
            return "Fine. Be that way!";

        var nonSpaceStatement = statement.Trim();
        var isQuestion = nonSpaceStatement.EndsWith('?');
        
        (var upperCasePresent, var lowerCasePresent) = Analyse(nonSpaceStatement);
        var isYelling = upperCasePresent && !lowerCasePresent;
        
        if (isYelling && isQuestion)
            return "Calm down, I know what I'm doing!";
        if (isYelling)
            return "Whoa, chill out!";
        if (isQuestion)
            return "Sure.";

        return "Whatever.";
/*
There are only 4 characteristics of the stament that you care about:
* Does it contain an uppercase letter?
* Does it contain a lower case letter?
* Is it entirely made up of whitespace?
* Is the last non-space character a '?'?

Not sure about the exlamation mark, '!' the tests don't push it.

The following may be useful:

    `string.Any(Char.IsUpper)`
    `string.Any(Char.IsLower)`
    `string.IsNullOrWhiteSpace()`


 */
      
    }
}