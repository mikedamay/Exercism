using System;
using System.Linq;

public static class RotationalCipher
{
    private const string codeLine = "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz";

    public static string Rotate(string text, int shiftKey)
    {
        var chars = text
            .Select(c => new {ch = Char.ToLower(c), upper = Char.IsUpper(c)})
            .Select(p => new {ch = Char.IsLower(p.ch) ? codeLine[p.ch - 'a' + shiftKey] : p.ch, 
                upper = p.upper})
            .Select(p => p.upper ? Char.ToUpper(p.ch) : p.ch).ToArray();
        
        return new string(chars);
    }
}

/*
Review Points:

That is a cool solution

Discussion Points:

If you're interested in discussing a more functional/LINQ-based solution then look at the community solutions and/or I would be happy to participate.
*/