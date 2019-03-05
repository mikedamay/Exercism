
using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;

public enum ConnectWinner
{
    White = 'O',
    Black = 'X',
    None = '.'
}
public class Connect
{
    private struct Location
    {
        public bool Left { get; }
        public bool Top { get; }
        public bool Right { get; }
        public bool Bottom { get; }

        public Location(bool left, bool top, bool right, bool bottom)
        {
            Left = left;
            Top = top;
            Right = right;
            Bottom = bottom;
        }
    }

    private readonly ReadOnlyDictionary<Location, Func<(int col, int row), IEnumerable<(int col, int row)>>>
        board = new ReadOnlyDictionary<Location, Func<(int col, int row), IEnumerable<(int col, int row)>>>(
            new Dictionary<Location, Func<(int col, int row), IEnumerable<(int col, int row)>>>
            {
                {new Location(true, true, true, true), cc => new (int, int)[0]},
                {new Location(true, true, true, false), cc => throw new Exception()},
                {new Location(true, true, false, true), cc => throw new Exception()},
                {new Location(true, false, true, true), cc => throw new Exception()},
                {new Location(false, true, true, true), cc => throw new Exception()},
                {new Location(false, true, false, true), cc => throw new Exception()},
                {new Location(false, true, true, true), cc => throw new Exception()},
                {new Location(false, false, false, false), cc => FullHouse(cc)},
                {
                    new Location(true, true, false, false),
                    cc => new (int, int)[] {(cc.col + 2, cc.row), (cc.col + 1, cc.row + 1)}
                },
                {
                    new Location(false, false, true, true),
                    cc => new (int, int)[] {(cc.col - 2, cc.row), (cc.col - 1, cc.row - 1)}
                },
                {
                    new Location(false, true, true, false),
                    cc => new (int, int)[] {(cc.col - 2, cc.row), (cc.col - 1, cc.row + 1), (cc.col + 1, cc.row + 1)}
                },
                {
                    new Location(true, false, false, true),
                    cc => new (int, int)[] {(cc.col + 2, cc.row), (cc.col - 1, cc.row - 1), (cc.col + 1, cc.row - 1)}
                },
                {
                    new Location(true, false, false, false),
                    cc => new (int, int)[]
                    {
                        (cc.col + 2, cc.row), (cc.col + 1, cc.row + 1), (cc.col - 1, cc.row - 1),
                        (cc.col + 1, cc.row - 1)
                    }
                },
                {
                    new Location(false, true, false, false),
                    cc => new (int, int)[]
                        {(cc.col + 2, cc.row), (cc.col - 2, cc.row), (cc.col - 1, cc.row + 1), (cc.col + 1, cc.row + 1)}
                },
                {
                    new Location(false, false, true, false),
                    cc => new (int, int)[]
                    {
                        (cc.col - 2, cc.row), (cc.col - 1, cc.row - 1), (cc.col - 1, cc.row + 1),
                        (cc.col + 1, cc.row + 1)
                    }
                },
                {
                    new Location(false, false, false, true),
                    cc => new (int, int)[]
                        {(cc.col + 2, cc.row), (cc.col - 2, cc.row), (cc.col - 1, cc.row - 1), (cc.col + 1, cc.row - 1)}
                },
            });
    private ConnectWinner winner;
    public Connect(string[] input)
    {
        ISet<(int, int)> visited = new HashSet<(int, int)>();            
        winner = input
            .First()
            .Select((c, idx) => new {stone = c, idx})
            .Where(p => p.stone == (char)ConnectWinner.Black)
            .Any(p => IsWinner(p.stone, (p.idx, 0), input, visited)) 
              ? ConnectWinner.Black 
              : ConnectWinner.None;
        if (winner == ConnectWinner.None)
            winner = input
                .LeftColumn()
                .Select((c, idx) => new {stone = c, idx})
                .Where(p => p.stone == (char)ConnectWinner.White)
                .Any(p => IsWinner(p.stone, (p.idx, 0), input, visited)) 
                ? ConnectWinner.White 
                : ConnectWinner.None;
            
    }

    public ConnectWinner Result()
    {
        return winner;
    }

    
    private bool IsWinner(char stone, (int col, int row) coords, string[] input, ISet<(int, int)> visited)
    {
        visited.Add(coords);
        if (IsTargetEdge(stone, coords, input))
            return true;
        return Neighbours(stone,  coords, input ).Where(cc => input[cc.row][cc.col] == stone)
            .Where(n => !visited.Contains(n))
            .Any(cc => IsWinner(stone, cc, input, visited))
    }

    private IEnumerable<(int col, int row)> Neighbours(
      char stone, (int col, int row) coords, string[] input)
    {
        if (IsLeftEdge(coords, input)
            && IsTopEdge(coords, input))
            return new (int col, int row)[] {(1, 1), (2, 1)};
    }

    private bool IsTargetEdge(char stone, (int col, int row) coords, string[] input)
    {
        if (stone == (char) ConnectWinner.Black)
        {
            return coords.row == input.Length;
        }
        else
        {
            return IsRightEdge(coords, input);
        }
    }

    /// <summary>
    /// returns neighbours for a non-edge stone 
    /// </summary>
    private static IEnumerable<(int col, int row)> FullHouse((int col, int row) coord)
    {
        yield return (coord.col - 1, coord.row - 1)
        yield return (coord.col + 1, coord.row - 1);
        yield return (coord.col - 2, coord.row);
        yield return (coord.col + 2, coord.row);
        yield return (coord.col - 1, coord.row + 1);
        yield return (coord.col + 1, coord.row + 1);
    }
    
    private bool IsRightEdge((int col, int row) coords, string[] input)
      => coords.col == AdjustedWidth(input) + coords.row;

    private bool IsLeftEdge((int col, int row) coords, string[] input)
      => coords.col == coords.row;

    private bool IsTopEdge((int col, int row) coords, string[] input)
        => coords.row == 0;

    private bool IsBottomEdge((int col, int row) coords, string[] input)
        => coords.row == input.Length - 1;
    
    private int AdjustedWidth(string[] input) => input.Length * 2 - 1;
}

public static class Extension
{
    public static IEnumerable<char> LeftColumn(this IEnumerable<string> input)
    {
        int row = 0;
        foreach (var line in input)
        {
            yield return line[row];    // the offset in chars of the rhombus matches the row number
        }
    }
}