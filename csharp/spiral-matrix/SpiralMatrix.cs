using System.Collections.Generic;
using System.Collections.Immutable;

public class SpiralMatrix
{
    private class Dims
    {
        public readonly int Min;
        public readonly int Max;
        public bool IsEmpty() => Min >= Max;
        public Dims(int min, int max)
        {
            this.Min = min;
            this.Max = max;
        }
    }
    public static int[,] GetMatrix(int size)
    {
        var list = WalkSpiral(0, 0, 1, new Dims(0, size), true);
        return list.ToSquareArray(size);
    }

    private static ImmutableList<(int, int, int)> WalkSpiral(int row, int col, int val, Dims dims, bool atStart)
    {
        int NextRow() => col == dims.Min && row != dims.Min ? row - 1 : col == dims.Max - 1 && row != dims.Max - 1 ? row + 1 : row;
        int NextCol() => row == dims.Min && col != dims.Max - 1 ? col + 1 : row == dims.Max - 1 && col != dims.Min ? col - 1 : col;
        bool IsSingleCell() => dims.Min == dims.Max - 1;
        
        if (dims.IsEmpty() || IsSingleCell() && !atStart)
        {
            return ImmutableList<(int, int, int)>.Empty;
        }
        if (NextRow() == dims.Min && NextCol() == dims.Min && !atStart)
        {
            var dims2 = new Dims(dims.Min + 1, dims.Max - 1);

            return WalkSpiral(dims2.Min, dims2.Min, val + 1, dims2, atStart: true).Add((row, col, val));
        }

        return WalkSpiral(NextRow(), NextCol(), val + 1, dims, atStart: false).Add((row, col, val));
    }
}

internal static class SpiralMatrixExtensions
{
    public static int[,] ToSquareArray(this IEnumerable<(int row, int col, int val)> list, int size)
    {
        var vals = new int[size, size];

        foreach (var item in list)
            vals[item.row, item.col] = item.val;
        return vals;
    }
}
