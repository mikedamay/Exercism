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
        
        var nonSpaceStatement = Regex.Replace(statement, @"\s+", string.Empty);
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

      
    }
}