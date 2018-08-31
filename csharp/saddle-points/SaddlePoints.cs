using System.Collections.Generic;

/// <summary>
/// This is rather sad, compared to maia13's linq solution.
/// Having seen that I could not be bothered to optimise this further
/// </summary>
public class SaddlePoints
{
    const byte ROW_DIM = 0;
    const byte COL_DIM = 1;
    const byte ROW_MAX_BIT = 1;
    const byte COL_MIN_BIT = 1 << 1;
    private readonly int[,] values;
    private byte[,] minAndMaxes;
    public SaddlePoints(int[,] values)
    {
        this.values = values;
        minAndMaxes = new byte[values.GetLength(0), values.GetLength(1)];
    }

    public IEnumerable<(int, int)> Calculate()
    {
        CalculateColMins();
        CalculateRowMaxes();
        List<(int, int)> saddlePoints = new List<(int, int)>();
        for (int ii = 0; ii < minAndMaxes.GetLength(ROW_DIM); ii++)
        {
            for (int jj = 0; jj < minAndMaxes.GetLength(COL_DIM); jj++)
            {
                if (minAndMaxes[ii, jj] != 0 && (COL_MIN_BIT | ROW_MAX_BIT) == (minAndMaxes[ii, jj] & (COL_MIN_BIT | ROW_MAX_BIT)))
                {
                    saddlePoints.Add((ii, jj));
                }
            }
        }

        return saddlePoints;
    }

    private void CalculateColMins()
    {
        for (int ii = 0; ii < values.GetLength(COL_DIM); ii++)
        {
            int? colMin = null;
            List<int> colIdxs = null;
            for (int jj = 0; jj < values.GetLength(ROW_DIM); jj++)
            {
                int cellVal = values[jj, ii];
                if (!colMin.HasValue || cellVal < colMin.Value)
                {
                    colMin = cellVal;
                    colIdxs = new List<int> {jj};
                }
                else if (cellVal == colMin.Value)
                {
                    colMin = cellVal;
                    colIdxs.Add(jj);
                }
            }

            for (int kk = 0; kk < colIdxs.Count; kk++)
            {
                minAndMaxes[colIdxs[kk], ii] |= COL_MIN_BIT;
            }
        }
    }
    private void CalculateRowMaxes()
    {
        for (int ii = 0; ii < values.GetLength(ROW_DIM); ii++)
        {
            int? rowMax = null;
            List<int> colIdxs = null;
            for (int jj = 0; jj < values.GetLength(COL_DIM); jj++)
            {
                int cellVal = values[ii, jj];
                if (!rowMax.HasValue || cellVal > rowMax.Value)
                {
                    rowMax = cellVal;
                    colIdxs = new List<int> {jj};
                }
                else if (cellVal == rowMax.Value)
                {
                    rowMax = cellVal;
                    colIdxs.Add(jj);
                }
            }

            for (int kk = 0; kk < colIdxs.Count; kk++)
            {
                minAndMaxes[ii, colIdxs[kk]] |= ROW_MAX_BIT;
            }
        }
    }
}