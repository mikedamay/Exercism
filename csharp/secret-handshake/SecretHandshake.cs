using System;
using System.Collections.Generic;
using System.Linq;

public static class SecretHandshake
{
    public static string[] Commands(int commandValue)
    {
        var commands = GenerateCommands(commandValue);
        return ((commandValue & (0b10000)) != 0 ? commands.Reverse() : commands).ToArray();
    }

    private static IEnumerable<string> GenerateCommands(int commandValue)
    {
        for (int ii = 0; ii < 4; ii++, commandValue >>= 1 )
        {
            if ((commandValue & 1) == 1)
            {
                switch (ii)
                {
                    case 0:
                        yield return "wink";
                        break;
                    case 1:
                        yield return "double blink";
                        break;
                    case 2:
                        yield return "close your eyes";
                        break;
                    case 3:
                        yield return "jump";
                        break;
                }
            }
        }
    }
}
