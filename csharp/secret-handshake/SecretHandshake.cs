using System.Collections.Generic;
using System.Linq;

public static class SecretHandshake
{
    public static string[] Commands(int commandValue)
    {
        var commands = GenerateCommands(commandValue);
        return ((commandValue & 0b10000) != 0 ? commands.Reverse() : commands).ToArray();
    }

    private static IEnumerable<string> GenerateCommands(int commandValue)
    {
        for (int ii = 0; ii < 4; ii++)
        {
            var cmdVal = commandValue & (1 << ii);
            switch (cmdVal)
            {
                case 0b1:
                    yield return "wink";
                    break;
                case 0b10:
                    yield return "double blink";
                    break;
                case 0b100:
                    yield return "close your eyes";
                    break;
                case 0b1000:
                    yield return "jump";
                    break;
            }
        }
    }
}
