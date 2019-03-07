using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

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

        private SafeSet(ImmutableHashSet<(int col, int row)> set) => this.set = set;

        public SafeSet Add((int col, int row) coords) => new SafeSet(set.Add(coords));

        public bool Contains((int col, int row) coords) => set.Contains(coords);

        public static SafeSet Empty => new SafeSet(ImmutableHashSet<(int col, int row)>.Empty);
    }

    private class SequenceResult<T>
    {
        public bool Succeeded { get; }
        private readonly T value;

        public SequenceResult(T value, bool succeeded)
        {
            this.value = value;
            Succeeded = succeeded;
        }

        public static implicit operator T(SequenceResult<T> _this) => _this.value;
    }
    
    private abstract class Sequence<T, U>
    {
        protected readonly IEnumerator<T> enumor;
        protected bool nextSucceeded;

        public Sequence(IEnumerator<T> enumor)
        {
            this.enumor = enumor;
            nextSucceeded = enumor.MoveNext();
        }

        public abstract U Next { get; }

        public abstract SequenceResult<T> Value { get; }
    }

    private class CoordSequence : Sequence<(int col, int row), CoordSequence>
    {
        public CoordSequence(IEnumerator<(int col, int row)> enumor) : base(enumor)
        {
        }
        
        public override CoordSequence Next => new CoordSequence(enumor);

        public override SequenceResult<(int col, int row)> Value 
            => new SequenceResult<(int col, int row)>(enumor.Current, nextSucceeded);
        
    }
    
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
        bool FindWinner(CoordSequence head,
            char playerStone, string[] input, SafeSet visited )
        {
            bool Find()
            {
/*
                var head2 = head.Next;
                var head3 = head2.Next;
                var head4 = head3.Next;
*/
                (bool succeeded, SafeSet visited, CoordSequence coords) candidate =
                    IsWinner(playerStone, head.Value, input, visited);                
                return candidate.succeeded || FindWinner(head.Next, playerStone, input, candidate.visited);
            }
            return head.Value.Succeeded && Find();
        }

        return FindWinner(new CoordSequence(startCoordsVectorArg.GetEnumerator()), playerStoneArg, inputArg,
            SafeSet.Empty);
    }

    private (bool succeeded, SafeSet visited, CoordSequence head) 
        IsWinner(char stone, SequenceResult<(int col, int row)> coords, string[] input
            , SafeSet visited)
    {
        IEnumerable<(int, int)> GetNeighbours() => Neighbours(coords, input).Where(cc => input[cc.row][cc.col] == stone)
            .Where(n => !visited.Contains(n));
        (bool, SafeSet, CoordSequence) IsWinnerGroup(CoordSequence head, SafeSet visited3)
        {
            (bool success, SafeSet visited, CoordSequence head) DoIsWinner()
            {
                var candidate = IsWinner(stone, head.Value, input, visited3);
                return candidate.succeeded ? candidate : IsWinnerGroup(head.Next, visited3);
            }

            return head.Value.Succeeded ? DoIsWinner() : (false, visited3, head.Next);
        }
        
        return IsTargetEdge(stone, coords, input) ?
            (true, null, null) :
            IsWinnerGroup(new CoordSequence(GetNeighbours().GetEnumerator()), visited.Add(coords));        
    }

    private IEnumerable<(int col, int row)> Neighbours((int col, int row) coords, string[] input)
      => AllAdjacentCoords(coords).Where(cc => IsValidCoord(cc, input));
    

    private bool IsValidCoord((int col, int row) cc, string[] input)
    {
        int TopEdge() => 0;
        int LeftEdge() => cc.row;
        int BottomEdge() => input.Length - 1;
        int RightEdge() => BoardWidth(input) + cc.row - 1;

        return cc.col >= LeftEdge() && cc.col <= RightEdge()
                                    && cc.row >= TopEdge() && cc.row <= BottomEdge();
    }

    private bool IsTargetEdge(char stone, (int col, int row) coords, string[] input)
      => stone == (char) ConnectWinner.Black
            ? IsRightEdge(coords, input)
            : IsBottomEdge(coords, input);

    /// <summary>
    /// returns neighbours for a non-edge stone 
    /// </summary>
    private static IEnumerable<(int col, int row)> AllAdjacentCoords((int col, int row) coord)
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
