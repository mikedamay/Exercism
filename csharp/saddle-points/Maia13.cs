using System;
using System.Collections.Generic;
using System.Linq;

public class SaddlePointsY
{
    private readonly int[,] matrix;
    public SaddlePointsY(int[,] values) => this.matrix = values;

    public IEnumerable<(int, int)> Calculate()
    {
        var matrixAsList = MatrixAsList();
        return matrixAsList.GroupBy(x => x.Row)
            .SelectMany(g => Filter(g, g.Max(x => x.Value)))
            .Intersect(matrixAsList.GroupBy(x => x.Col)
                .SelectMany(g => Filter(g, g.Min(x => x.Value))))
            .Select(x => (x.Row, x.Col));
    }

    private IEnumerable<(int Row, int Col, int Value)> MatrixAsList()
    {
        for (int row = 0; row < matrix.GetLength(0); row++)
        {            
            for (int col = 0; col < matrix.GetLength(1); col++)
            {
                yield return (row, col, matrix[row, col]);
            }
        }
    }

    private IEnumerable<(int Row, int Col, int Value)> Filter(IGrouping<int, (int Row, int Col, int Value)> collection, int filterValue) => collection.Where(x => x.Value == filterValue);

}