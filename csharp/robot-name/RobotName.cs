using System;

public class Robot
{
    public string name;
    private object random;
    private object rng;
    private string[] text = { "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z" };
    private string[] digit = {"1", "2", "3", "4", "5", "6", "7", "8", "9"};
   
    public string Name
    {
        get
        {
            return name;
        }

        set
        {
            name = (GetRandomCharacter() + GetRandomCharacter () + 
        }
    }

    public void Reset()
    {
        
    }

    public static char GetRandomCharacter(string text, Random rng)
    {
        int index = rng.Next(text.Length);
        return text[index];
    }
    public static char GetRandomDigit(string digit, Random rng)
    {
        int index = rng.Next(digit.Length);
        return digit[index];
    }
}