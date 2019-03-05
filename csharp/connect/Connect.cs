using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.Design;
using System.Diagnostics;
using System.Linq;
using System.Reflection;
using System.Text;
using Xunit;




public enum ConnectWinner
{
    White,
    Black,
    None
}

public class Connect
{
    private enum Dim
    {
        X = 0,
        Y = 1
    }
    private enum CellStatus
    {
        White = 'O',
        Black = 'X',
        Free = '.',
        Separator = '|',
        Margin = '^',
        OffRhombus = ' ',
    }

    private class CellId : ICloneable
    {
        public readonly int row;
        public readonly int col;

        public CellId(int row, int col)
        {
            this.row = row;
            this.col = col;
        }

        public Object Clone()
        {
            return new CellId(this.row, this.col);
        }
    }
    private class Cell
    {
        public readonly CellId Id;
        public readonly CellId[] Connections;

        public CellStatus Status { get; set; }

        private readonly Side Side;
        
        public Cell(CellId id, CellStatus cellStatus, Side side, CellId[] connections)
        {
            Id = id;
            Connections = (CellId[])connections.Clone();
            this.Side = side;
            this.Status = cellStatus;
        }

        public bool IsOnSide(Side side) => this.Side.HasFlag(side);

    }

    [Flags]
    private enum Side : Byte
    {
        None = 0,
        Top = 1 << 0,
        Left = 1 << 1,
        Bottom = 1 << 2,
        Right = 1 << 4,
    }

    private const int X_MARGIN = 2;
    private const int Y_MARGIN = 1;

    /**
      The essential mechanism in the program is the grid created within the game.
      cells are mapped from the "game matrix" to the grid.  as you see below
      the grid contains the shape of the rhombus.  It contains a margin
      such that connections to neighbouring cells from anywhere in the game area
      will result in a non-null cell being found
      
        ^^^^^^^^^^^^^^^
        ^^.|O|.|.    ^^
        ^^ O|X|X|X   ^^
        ^^  O|X|O|.  ^^
        ^^   X|X|O|X ^^
        ^^    .|O|X|.^^
        ^^^^^^^^^^^^^^^
        
      The grid concept is almost completely encapsulated within Game (there is a
      slight leak in the CellStatus enum).  However within Game the relationship
      between game matrix and grid is distributed
      between MapCellFromGameToGrid() and other methods such as CalculateSide(),
      CalculateCellStatus() and CalculateGridSize()
     */
    private class Game
    {
        public readonly Side WhiteSide;
        public readonly Side BlackSide;
        public readonly int GameRows;
        public readonly int GameCols;

        private readonly Cell[,] grid;
        
        public Game(Side whiteSide, Side blackSide, int cols, int rows)
        {
            GameCols = cols;
            GameRows = rows;
            WhiteSide = whiteSide;
            BlackSide = blackSide;
            grid = GenerateEmptyGrid(cols, rows);
        }

        public Cell this[int gameCellX, int gameCellY]
        {
            get
            {
                (int gridCellX, int gridCellY) = MapCellFromGameToGrid(gameCellX, gameCellY);
                return this.grid[gridCellX, gridCellY];
            }
        }

        public Cell this[(int col, int row) cellSpec]
          => this[cellSpec.col, cellSpec.row];

        public IEnumerable<Cell> GetNeighbours(Cell cellArg, ConnectWinner player)
        {
            foreach (CellId connectionId in cellArg.Connections)
            {
                Cell cell = grid[connectionId.row, connectionId.col];
                CellStatus playerStatus = player == ConnectWinner.White ? CellStatus.White : CellStatus.Black;
                if (cell.Status == playerStatus)
                {
                    yield return cell;
                }
            }
        }
        private (int gridCellX, int gridCellY) MapCellFromGameToGrid(int gameCellX, int gameCellY)
        {
            var rhombusOffset = gameCellY + X_MARGIN;
            return (gameCellX * 2 + rhombusOffset, gameCellY + Y_MARGIN);
        }

        /// <param name="gameCellsAcross">(number of game cells across * 2 - 1) * 2 + 2</param>
        /// <param name="gameCellsDown">number of game cells down * 2 + 2</param>
        /// <returns></returns>
        private Cell[,] GenerateEmptyGrid(int gameCellsAcross, int gameCellsDown)
        {
            (int gridCellsAcross, int gridCellsDown) = CalculateGridSize(gameCellsAcross, gameCellsDown); 
            var grid = new Cell[gridCellsAcross, gridCellsDown];
            for (int col = 0; col < gridCellsAcross; col++)
            {
                for (int row = 0; row < gridCellsDown; row++)
                {
                    var cellStatus = CalculateCellStatus(col, row, gridCellsAcross, gridCellsDown);
                    var side = CalculateSide(col, row, gridCellsAcross, gridCellsDown);
                    
                    grid[col,row] = new Cell(new CellId(col, row), cellStatus, side
                      , GetConnections(col, row, cellStatus));
                }
            }
            return grid;
        }

        /// <summary>
        /// if the cell lies on one or more sides of the game matrix then
        /// these sides are returned as a bit mask.
        /// NOote that the actual calculation is done on the grid matrix rather than the game matrix
        /// </summary>
        /// <param name="col">grid co-ordinates</param>
        /// <param name="row">grid co-ordinates</param>
        /// <returns>a mask that comprises Side.None or a combination of the
        /// 4 substantive side enums</returns>
        private Side CalculateSide(int col, int row, int cellsAcross, int cellsDown)
        {
            Side side = Side.None;
            if (col == X_MARGIN)
                side |= Side.Top;
            if (row == Y_MARGIN)
                side |= Side.Left;
            if (col == cellsAcross - cellsDown - Y_MARGIN + row)
                side |= Side.Right;
            if (row == cellsDown - Y_MARGIN - 1)
                side |= Side.Bottom;
            return side;
        }

        private CellStatus CalculateCellStatus(int xx, int yy, int cellsAcross, int cellsDown)
        {
            if (xx < X_MARGIN || xx >= cellsAcross - X_MARGIN
              || yy < Y_MARGIN || yy >= cellsDown - Y_MARGIN)
            {
                return CellStatus.Margin;        // outer margine of board
            }

            if (xx < X_MARGIN + yy - Y_MARGIN)
            {
                return CellStatus.OffRhombus;        // left slope of rhombus
            }

            if (xx > cellsAcross - cellsDown - Y_MARGIN + yy)
            {
                return CellStatus.OffRhombus;        // right slope of rhombus
            }

            var alignment = yy % 2 == 1 ? 0 : 1;
            return xx % 2 == alignment ? CellStatus.Free : CellStatus.Separator;
            
        }

        /// <summary>
        /// x-dim accounts for separators (accross * 2 - 1), the right shift (down - 1) and end guards (+4)  
        /// </summary>
        private (int, int) CalculateGridSize(int gameCellsAcross, int gameCellsDown)
        {
            return (gameCellsAcross * 2 - 1 + gameCellsDown - 1 + X_MARGIN * 2, gameCellsDown + Y_MARGIN * 2);
        }

        /// <summary>
        /// Each cell in the playing area will end up with 6 connections albeit that
        /// some of the connections for edge and corner cells will be to cells that
        /// are outside the playing area.  This will make it easy to test where
        /// edges have been reached.
        /// </summary>
        /// <param name="col">grid position</param>
        /// <param name="row">grid position</param>
        /// <returns>0 - 6 neighbour cells</returns>
        private CellId[] GetConnections(int col, int row, CellStatus cellStatus)
        {
            var connections = new List<CellId>();
            if (cellStatus == CellStatus.Free)
            {
                connections.Add(new CellId(col - 2, row));
                connections.Add(new CellId(col + 2, row));
                connections.Add(new CellId(col + 1, row + 1));
                connections.Add(new CellId(col - 1, row + 1));
                connections.Add(new CellId(col + 1, row - 1));
                connections.Add(new CellId(col - 1, row - 1));
            }
            return connections.ToArray();
        }

        public override string ToString()
        {
            var sb = new StringBuilder();
            for (int ii = 0; ii <= grid.GetUpperBound((int)Dim.Y); ii++)
            {
                for (int jj = 0; jj <= grid.GetUpperBound((int)Dim.X); jj++)
                {
                    sb.Append((char)grid[jj, ii].Status);
                }

                sb.Append('\n');
            }

            return sb.ToString();
        }
    }        // Game

    private readonly Game game;
    
    public Connect(string[] input)
    {
        game = MakeGame(input);
    }

    private Game MakeGame(string[] input)
    {
        ValidateInput(input);
        int gameCellsDown = input.Length;
        int gameCellsAcross = input[0].Split(' ', StringSplitOptions.RemoveEmptyEntries).Length;
        Game game = new Game(Side.Top, Side.Left, gameCellsAcross, gameCellsDown);
        for (int row = 0; row < input.Length; row++)
        {
            var cols = input[row].Split(' ', StringSplitOptions.RemoveEmptyEntries);
            for (int col = 0; col < cols.Length; col++)
            {
                Cell cell = game[col, row];
                MyDebug.Assert(cell.Status == CellStatus.Free);
                cell.Status = Enum.Parse<CellStatus>(((int)cols[col][0]).ToString());
                MyDebug.Assert(cell.Status == CellStatus.Free
                               || cell.Status == CellStatus.White
                               || cell.Status == CellStatus.Black);
            }                            
        }
        return game;
    }

    public ConnectWinner Result()
    {
        if (IsWinner(ConnectWinner.White))
        {
            return ConnectWinner.White;
        }
        else if (IsWinner(ConnectWinner.Black))
        {
            return ConnectWinner.Black;
        }
        else
        {
            return ConnectWinner.None;
        }
    }

    private bool IsWinner(ConnectWinner player)
    {
        (int col, int row)[] starts 
          = GetStartingCellPositions(
            player == ConnectWinner.White ? Side.Top : Side.Left);
        foreach ((int, int) start in starts)
        {
            Cell cell = game[start];
            MyDebug.Assert(cell.Status == CellStatus.Free 
                           || cell.Status == CellStatus.Black 
                           || cell.Status == CellStatus.White);
            CellStatus requiredStatus = player == ConnectWinner.Black ? CellStatus.Black : CellStatus.White;
            if (cell.Status == requiredStatus)
            {
                if (IsCellConnectedTo(cell, player, player == ConnectWinner.White ? Side.Bottom : Side.Right))
                    return true;
            }
        }

        return false;
    }

    /// <returns>an array of cell coordinates including the 2 corners for the specified edge</returns>
    private (int col, int row)[] GetStartingCellPositions(Side side)
    {
        if (side == Side.Top)
        {
            IList<(int, int)> list = new List<(int, int)>();
            for (int ii = 0; ii < game.GameCols; ii++)
            {
                list.Add((ii, 0));
            }

            return list.ToArray();
        }
        else if (side == Side.Left)
        {
            IList<(int, int)> list = new List<(int, int)>();
            for (int ii = 0; ii < game.GameRows; ii++)
            {
                list.Add((0, ii));
            }

            return list.ToArray();
        }
        MyDebug.Assert(false);    // only top and left are supported
        return new (int col, int row)[0];
    }

    private bool IsCellConnectedTo(Cell cellArg, ConnectWinner player, Side targetSide)
    {
        ISet<Cell> visited = new HashSet<Cell>();

        bool IsCellConnected(Cell cell)
        {
            if (cell.IsOnSide(targetSide))
                return true;
            foreach (Cell neighbour in game.GetNeighbours(cell, player))
            {
                if (visited.Contains(neighbour))
                    continue;
                visited.Add(cell);
                if (IsCellConnected(neighbour))
                    return true;
            }

            return false;
        }

        return IsCellConnected(cellArg);
    }
    private void ValidateInput(string[] input)
    {
        if (input == null || input.Length == 0)
            throw new ArgumentException();
        var len = input[0].Split(' ', StringSplitOptions.RemoveEmptyEntries).Length;
        foreach (var line in input)
        {
            var parts = line.Split(' ', StringSplitOptions.RemoveEmptyEntries);
            if (len != parts.Length
                || line.Replace(" ", string.Empty)
                    .Except(GetValidCellContent()).Any())
            {
                throw new ArgumentException();
            }
        }
    }

    private IEnumerable<char> GetValidCellContent()
    {
        yield return (char)CellStatus.Free;
        yield return (char)CellStatus.White;
        yield return (char) CellStatus.Black;
    }
}

internal class MyDebug
{
    public static void Assert(bool b)
    {
        if (!b) throw new Exception();
    }
}