using System;
using System.Text;

namespace NonLinq
{
    public class RotationalCypher
    {
        private const string codeLine = "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz";
        public static string Rotate(string text, int shiftKey)
        {
            var sb = new StringBuilder();
            foreach (var ch in text)
            {
                bool uppser = Char.IsUpper(ch);
                var chLower = Char.ToLower(ch);
                if (chLower < 'a' || chLower > 'z')
                    sb.Append(ch);
                else
                {
                    var chOut = codeLine[chLower - 'a' + shiftKey];
                    sb.Append(uppser ? Char.ToUpper(chOut) : chOut);
                }
            }

            return sb.ToString();
        }
        
    }
}