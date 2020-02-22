using System;
using System.Linq;
using System.Text;


public static class ReverseString2
{
	public static string Reverse(string input)
	{
		return new string(input.Reverse().ToArray());
	}
}
