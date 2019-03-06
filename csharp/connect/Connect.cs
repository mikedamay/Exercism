
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Collections.ObjectModel;
using System.Linq;

public enum ConnectWinner
{
    White = 'O',
    Black = 'X',
    None = 'X' + 1
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
                {new Location(left :true, top: true, right: true, bottom: true), cc => new (int, int)[0]},
                {new Location(left :true, top: true, right: true, bottom: false), cc => throw new Exception()},
                {new Location(left :true, top: true, right: false, bottom: true), cc => throw new Exception()},
                {new Location(left :true, top: false, right: true, bottom: false), cc => throw new Exception()},
                {new Location(left :true, top: false, right: true, bottom: true), cc => throw new Exception()},
                {new Location(left :false, top: true, right: false, bottom: true), cc => throw new Exception()},
                {new Location(left :false, top: true, right: true, bottom: true), cc => throw new Exception()},
                {new Location(left :false, top: false, right: false, bottom: false), cc => FullHouse(cc)},
                {
                    new Location(left :true, top: true, right: false, bottom: false),
                    cc => new (int, int)[] {(cc.col + 2, cc.row), (cc.col + 1, cc.row + 1)}
                },
                {
                    new Location(left :false, top: false, right: true, bottom: true),
                    cc => new (int, int)[] {(cc.col - 2, cc.row), (cc.col - 1, cc.row - 1)}
                },
                {
                    new Location(left :false, top: true, right: true, bottom: false),
                    cc => new (int, int)[] {(cc.col - 2, cc.row), (cc.col - 1, cc.row + 1), (cc.col + 1, cc.row + 1)}
                },
                {
                    new Location(left :true, top: false, right: false, bottom: true),
                    cc => new (int, int)[] {(cc.col + 2, cc.row), (cc.col - 1, cc.row - 1), (cc.col + 1, cc.row - 1)}
                },
                {
                    new Location(left :true, top: false, right: false, bottom: false),
                    cc => new (int, int)[]
                    {
                        (cc.col + 2, cc.row), (cc.col + 1, cc.row + 1), (cc.col - 1, cc.row - 1),
                        (cc.col + 1, cc.row - 1)
                    }
                },
                {
                    new Location(left :false, top: true, right: false, bottom: false),
                    cc => new (int, int)[]
                        {(cc.col + 2, cc.row), (cc.col - 2, cc.row), (cc.col - 1, cc.row + 1), (cc.col + 1, cc.row + 1)}
                },
                {
                    new Location(left :false, top: false, right: true, bottom: false),
                    cc => new (int, int)[]
                    {
                        (cc.col - 2, cc.row), (cc.col - 1, cc.row - 1), (cc.col - 1, cc.row + 1),
                        (cc.col + 1, cc.row + 1)
                    }
                },
                {
                    new Location(left :false, top: false, right: false, bottom: true),
                    cc => new (int, int)[]
                        {(cc.col + 2, cc.row), (cc.col - 2, cc.row), (cc.col - 1, cc.row - 1), (cc.col + 1, cc.row - 1)}
                },
            });
    private ConnectWinner winner;
    public Connect(string[] input)
    {
        ISet<(int, int)> visited = new HashSet<(int, int)>();
        ImmutableHashSet<(int, int)> visit = ImmutableHashSet<(int, int)>.Empty;
        winner = input
            .LeftColumn()
            .Select((c, idx) => new {stone = c, idx})
            .Where(p => p.stone == (char) ConnectWinner.Black)
            .Any(p => IsWinner(p.stone, (p.idx, p.idx), input, visited))
            ? ConnectWinner.Black
            : input
                .First()
                .Select((c, idx) => new {stone = c, idx})
                .Where(p => p.stone == (char) ConnectWinner.White)
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
        MyDebug.Assert(input[coords.row][coords.col] == stone);
        visited.Add(coords);
        if (IsTargetEdge(stone, coords, input))
            return true;
        return Neighbours(coords, input).Where(cc => input[cc.row][cc.col] == stone)
            .Where(n => !visited.Contains(n))
            .Any(cc => IsWinner(stone, cc, input, visited));
    }

    private IEnumerable<(int col, int row)> Neighbours((int col, int row) coords, string[] input)
    {
        var location = new Location(IsLeftEdge(coords, input), IsTopEdge(coords, input)
            , IsRightEdge(coords, input), IsBottomEdge(coords, input));
        return this.board[location](coords);
    }

    private bool IsTargetEdge(char stone, (int col, int row) coords, string[] input)
      => stone == (char) ConnectWinner.Black
            ? IsRightEdge(coords, input)
            : IsBottomEdge(coords, input);

    /// <summary>
    /// returns neighbours for a non-edge stone 
    /// </summary>
    private static IEnumerable<(int col, int row)> FullHouse((int col, int row) coord)
    {
        yield return (coord.col - 1, coord.row - 1);
        yield return (coord.col + 1, coord.row - 1);
        yield return (coord.col - 2, coord.row);
        yield return (coord.col + 2, coord.row);
        yield return (coord.col - 1, coord.row + 1);
        yield return (coord.col + 1, coord.row + 1);
    }
    
    private bool IsRightEdge((int col, int row) coords, string[] input)
      => coords.col == BoardWidth(input) + coords.row - 1;

    private bool IsLeftEdge((int col, int row) coords, string[] input)
      => coords.col == coords.row;

    private bool IsTopEdge((int col, int row) coords, string[] input)
        => coords.row == 0;

    private bool IsBottomEdge((int col, int row) coords, string[] input)
        => coords.row == input.Length - 1;
    
    private int BoardWidth(string[] input) => input[0].Length;

}

public static class Extension
{
    public static IEnumerable<char> LeftColumn(this IEnumerable<string> input)
    {
        int row = 0;
        foreach (var line in input)
        {
            yield return line[row++];    // the offset in chars of the rhombus matches the row number
        }
    }
}

internal class MyDebug
{
    public static void Assert(bool b)
    {
        if (!b) throw new Exception();
    }
}
