
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Collections.ObjectModel;
using System.Linq;
using System.Reflection.Metadata.Ecma335;

public enum ConnectWinner
{
    White = 'O',
    Black = 'X',
    None = 'X' + 1
}
public class Connect
{
    private class SafeSet
    {
        private ImmutableHashSet<(int col, int row)> set;

        private SafeSet(ImmutableHashSet<(int col, int row)> set)
        {
            this.set = set;
        }

        public SafeSet Add((int col, int row) coords)
        {
            return new SafeSet(set.Add(coords));
        }

        public bool Contains((int col, int row) coords)
        {
            return set.Contains(coords);
        }

        public static SafeSet Empty => new SafeSet(ImmutableHashSet<(int col, int row)>.Empty);
    }
    
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

    private static readonly ReadOnlyDictionary<Location, Func<(int col, int row), IEnumerable<(int col, int row)>>>
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
        winner = FindWinner(input
            .LeftColumn()
            .Select((c, idx) => new {stone = c, idx})
            .Where(p => p.stone == (char) ConnectWinner.Black)
            .Select(p => (p.idx, p.idx))
            , (char)ConnectWinner.Black, input)
            ? ConnectWinner.Black
            : FindWinner(input
                .First()
                .Select((c, idx) => new {stone = c, idx})
                .Where(p => p.stone == (char) ConnectWinner.White)
                .Select(p => (p.idx, 0))
                , (char)ConnectWinner.White, input)
                ? ConnectWinner.White
                : ConnectWinner.None;            
    }

    public ConnectWinner Result()
    {
        return winner;
    }

    private bool FindWinner(IEnumerable< (int col, int row)> startCoordsVectorArg, char playerStoneArg, string[] inputArg)
    {
        bool FindWinner(IEnumerator<(int col, int row)> startCoordsVector,
            char playerStone, string[] input, SafeSet visited )
        {
            bool Find()
            {
                (bool result, SafeSet visited) candidate =
                    IsWinner(playerStone, startCoordsVector.Current, input, visited);
                return candidate.result || FindWinner(startCoordsVector, playerStone, input, candidate.visited);
            }
            return startCoordsVector.MoveNext() && Find();
        }

        return FindWinner(startCoordsVectorArg.GetEnumerator(), playerStoneArg, inputArg,
            SafeSet.Empty);
    }

    private (bool, SafeSet) 
        IsWinner(char stone, (int col, int row) coords, string[] input
            , SafeSet visited)
    {
        IEnumerable<(int, int)> GetNeighbours() => Neighbours(coords, input).Where(cc => input[cc.row][cc.col] == stone)
            .Where(n => !visited.Contains(n));
        (bool, SafeSet) IsWinnerGroup(IEnumerator<(int col, int row)> neighbours, SafeSet visited3)
        {
            (bool success, SafeSet visited) DoIsWinner()
            {
                var candidate = IsWinner(stone, neighbours.Current, input, visited3);
                return candidate.Item1 ? candidate : IsWinnerGroup(neighbours, visited3);
            }

            return neighbours.MoveNext() ? DoIsWinner() : (false, visited3);
        }
        
        return IsTargetEdge(stone, coords, input) ?
            (true, visited.Add(coords)) :
            IsWinnerGroup(GetNeighbours().GetEnumerator(), visited.Add(coords));        
    }

    private IEnumerable<(int col, int row)> Neighbours((int col, int row) coords, string[] input)
    {
        var location = new Location(IsLeftEdge(coords, input), IsTopEdge(coords, input)
            , IsRightEdge(coords, input), IsBottomEdge(coords, input));
        return board[location](coords);
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
