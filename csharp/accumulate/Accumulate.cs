using System;
using System.Collections.Generic;

public static class AccumulateExtensions
{
    public static IEnumerable<U> Accumulate<T, U>(this IEnumerable<T> collection, Func<T, U> fun)
    {
        foreach(var item in collection)
        {
            yield return fun(item);
        }
    }
}