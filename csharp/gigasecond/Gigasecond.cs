using System;

public static class Gigasecond
{
    public static DateTime Add(DateTime birthDate)
    {
        return birthDate.AddSeconds(1e9);
    }
}

/*
Review Points: 
 
Good solution

Discussion Points:

It could be useful for you to know about the csharp comma separator _ as in `int num = 1_000_000_000;`

*/