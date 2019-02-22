using System;
using System.Linq;
using System.Text;

public static class RotationalCipher
{
    private const string codeLine = "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz";
    public static string Rotate(string text, int shiftKey)
    {
        var expr = text
            .Select(c => ((char ch, bool upper))(Char.ToLower(c), Char.IsUpper(c)))
            .Select(p => ((char ch, bool upper))(Char.IsLower(p.ch) ? codeLine[p.ch - 'a' + shiftKey] : p.ch, p.upper))
            .Select(p => p.upper ? Char.ToUpper(p.ch) : p.ch).ToArray();
        
        return new string(expr);
    }
}