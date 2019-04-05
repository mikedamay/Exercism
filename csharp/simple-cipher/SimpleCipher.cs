using System;
using System.Linq;
using System.Text;

public class SimpleCipher
{
    private static readonly Random rand = new Random();
    private const string alphabet = "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz";
    private static readonly string reverseAlphabet = string.Join(string.Empty, alphabet.Reverse());
    
    public SimpleCipher() 
      : this(string.Join(string.Empty, Enumerable.Range(0, 100)
          .Select( _ => (char)rand.Next('a', 'z' + 1))))
    {

    }

    public SimpleCipher(string key)
    {
        Key = key;
    }
    
    public string Key 
    {
        get;
    }

    public string Encode(string plaintext)
    {
        var sb = new StringBuilder();
        for (int ii = 0; ii < plaintext.Length; ii++)
        {
            int shifter = ii % Key.Length;
            char cipherChar = alphabet[plaintext[ii] - 'a' + Key[shifter] - 'a'];
            sb.Append(cipherChar);
        }

        return sb.ToString();
    }

    public string Decode(string ciphertext)
    {
        var sb = new StringBuilder();
        for (int ii = 0; ii < ciphertext.Length; ii++)
        {
            int shifter = ii % Key.Length;
            char cipherChar = reverseAlphabet[ciphertext[ii] - 'a' + Key[shifter] - 'a'];
            sb.Append(cipherChar);
        }

        return sb.ToString();
    }
}