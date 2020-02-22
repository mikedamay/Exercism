using System;
using System.Text;

public static class ReverseString
{
    public static string Reverse(string input)
    {
        //var sb = new StringBuilder(input ?? string.Empty);
		var sbOut = new StringBuilder();
		for (int ii = 0, jj = input.Length - 1; ii < input.Length; ii++, jj--)
		{
			sbOut.Append(input[jj]);
			if (Char.IsHighSurrogate(input[jj]))
			{
				(sbOut[ii], sbOut[ii - 1]) = (sbOut[ii - 1], sbOut[ii]);
			}
		}
		return sbOut.ToString();
    }
}
