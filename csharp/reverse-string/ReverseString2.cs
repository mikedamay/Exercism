using System;
using System.Text;


public static class ReverseString2
{
	public static string Reverse(string input)
	{
		ReadOnlySpan<char> ros = input.AsSpan();
		return new string(ros.ToArray());
	}
}
