using System;
using System.Linq;

public class PhoneNumber
{
    public static string Clean(string phoneNumber)
    {
        var str = string.Concat(phoneNumber
            .Where(Char.IsDigit)
            .Where((c, idx) => !(idx == 0 && c == '1'))
            .Select((c, idx) => (idx == 0 || idx == 3)  && (c == '0' || c == '1') ? throw new ArgumentException() : c )
        );
        return str.Length == 10 ? str : throw new ArgumentException();
    }
}