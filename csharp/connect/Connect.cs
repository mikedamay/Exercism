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
        public readonly int x;
        public readonly int y;

        public CellId(int x, int y)
        {
            this.x = x;
            this.y = y;
        }

        public Object Clone()
        {
            return new CellId(this.x, this.y);
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
    
    private class Game
    {
        public readonly Side WhiteSide;
        public readonly Side BlackSide;
        public readonly Cell[,] board;
        public readonly int GameRows;
        public readonly int GameCols;

        public Game(Side whiteSide, Side blackSide, int width, int height)
        {
            GameCols = width;
            GameRows = height;
            WhiteSide = whiteSide;
            BlackSide = blackSide;
            board = GenerateEmptyBoard(width, height);
        }

        public Cell this[int gameCellX, int gameCellY]
        {
            get
            {
                (int boardCellX, int boardCellY) = MapCellFromGameToBoard(gameCellX, gameCellY);
                return this.board[boardCellX, boardCellY];
            }
        }

        public Cell this[(int col, int row) cellSpec]
          => this[cellSpec.col, cellSpec.row];

        public IEnumerable<Cell> GetNeighbours(Cell cellArg, ConnectWinner player)
        {
            foreach (CellId connectionId in cellArg.Connections)
            {
                Cell cell = board[connectionId.x, connectionId.y];
                CellStatus playerStatus = player == ConnectWinner.White ? CellStatus.White : CellStatus.Black;
                if (cell.Status == playerStatus)
                {
                    yield return cell;
                }
            }
        }
        private (int, int) MapCellFromGameToBoard(int gameCellX, int gameCellY)
        {
            var rhombusOffset = gameCellY + X_MARGIN;
            return ((gameCellX) * 2 + rhombusOffset, gameCellY + Y_MARGIN);
        }

        /// </summary>
        /// <param name="gameCellsAcross">(number of game cells across * 2 - 1) * 2 + 2</param>
        /// <param name="gameCellsDown">number of game cells down * 2 + 2</param>
        /// <returns></returns>
        private Cell[,] GenerateEmptyBoard(int gameCellsAcross, int gameCellsDown)
        {
            (int cellsAcross, int cellsDown) = CalculateBoardSize(gameCellsAcross, gameCellsDown); 
            var board = new Cell[cellsAcross, cellsDown];
            for (int xx = 0; xx < cellsAcross; xx++)
            {
                for (int yy = 0; yy < cellsDown; yy++)
                {
                    var cellStatus = CalculateCellStatus(xx, yy, cellsAcross, cellsDown);
                    var side = CalculateSide(xx, yy, cellsAcross, cellsDown);
                    
                    board[xx,yy] = new Cell(new CellId(xx, yy), cellStatus, side
                      , GetConnections(xx, yy, cellsAcross, cellsDown, cellStatus));
                }
            }
            return board;
        }

        private Side CalculateSide(int xx, int yy, int cellsAcross, int cellsDown)
        {
            Side side = Side.None;
            if (xx == X_MARGIN)
                side |= Side.Top;
            if (yy == Y_MARGIN)
                side |= Side.Left;
            if (xx == cellsAcross - cellsDown - Y_MARGIN + yy)
                side |= Side.Right;
            if (yy == cellsDown - Y_MARGIN - 1)
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
        private (int, int) CalculateBoardSize(int gameCellsAcross, int gameCellsDown)
        {
            return (gameCellsAcross * 2 - 1 + gameCellsDown - 1 + X_MARGIN * 2, gameCellsDown + Y_MARGIN * 2);
        }

        private CellId[] GetConnections(int xx, int yy, int width, int height, CellStatus cellStatus)
        {
            var connections = new List<CellId>();
            if (cellStatus == CellStatus.Free)
            {
                connections.Add(new CellId(xx - 2, yy));
                connections.Add(new CellId(xx + 2, yy));
                connections.Add(new CellId(xx + 1, yy + 1));
                connections.Add(new CellId(xx - 1, yy + 1));
                connections.Add(new CellId(xx + 1, yy - 1));
                connections.Add(new CellId(xx - 1, yy - 1));
            }
            return connections.ToArray();
        }

        public string ToString()
        {
            var sb = new StringBuilder();
            for (int ii = 0; ii <= board.GetUpperBound((int)Dim.Y); ii++)
            {
                for (int jj = 0; jj <= board.GetUpperBound((int)Dim.X); jj++)
                {
                    sb.Append((char)board[jj, ii].Status);
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
        else
        {
            IList<(int, int)> list = new List<(int, int)>();
            for (int ii = 0; ii < game.GameRows; ii++)
            {
                list.Add((0, ii));
            }

            return list.ToArray();
        }
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
}

internal class MyDebug
{
    public static void Assert(bool b)
    {
        if (!b) throw new Exception();
    }
}