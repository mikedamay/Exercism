using System;
using System.Linq.Expressions;
using Xunit;

namespace ExerciseReport
{
    public static class MyDebug
    {
        public static void Assert(Expression<Func<bool>> expr, string message)
        {
            var fun = expr.Compile();
            if (!fun())
            {
                throw new InvalidOperationException($"{message}: {expr}");
            }
        }
    }
}