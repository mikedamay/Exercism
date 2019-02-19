using System;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using System.Net.Sockets;

public static class ResistorColor
{
    enum Color
    {
        black,
        brown,
        red,
        orange,
        yellow,
        green,
        blue,
        violet,
        grey,
        white
        
    }
    public static int ColorCode(string color)
    {
        return (int)Enum.Parse(typeof(Color), color);
    }

    public static string[] Colors()
    {
        return Enum.GetNames(typeof(Color));        
    }
}