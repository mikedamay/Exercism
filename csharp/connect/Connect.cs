using System;
using System.Collections.Generic;
using System.Text;

public enum ConnectWinner
{
    White,
    Black,
    None
}

public class Connect
{
    private enum CellStatus
    {
        White = 'O',
        Black = 'X',
        Free = '.',
        Separator = '|',
        Margin = '^',
        OffRhombus = ' ',
    }

    private class CellId
    {
        public readonly int x;
        public readonly int y;

        public CellId(int x, int y)
        {
            this.x = x;
            this.y = y;
        }
    }
    private class Cell
    {
        public readonly CellId Id;
        public readonly CellId[] Connections;
        public CellStatus Status { get; set; }
        public bool VisitedByWhite { get; set; }
        public bool VisiteyBlack { get; set; }

        public Cell(CellId id, CellStatus cellStatus, CellId[] connections)
        {
            Id = id;
            Connections = (CellId[])connections.Clone();
            this.Status = cellStatus;
        }
    }

    private enum Side
    {
        Top,
        Left
    }

    private const int X_MARGIN = 2;
    private const int Y_MARGIN = 1;
    
    private class Game
    {
        public readonly Side WhiteSide;
        public readonly Side BlackSide;
        public readonly Cell[,] board;

        public Game(Side whiteSide, Side blackSide, int width, int height)
        {
            WhiteSide = whiteSide;
            BlackSide = blackSide;
            board = GenerateEmptyBoard(width, height);
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
                    board[xx,yy] = new Cell(new CellId(xx, yy), cellStatus
                      , GetConnections(xx, yy, cellsAcross, cellsDown, cellStatus));
                }
            }
            return board;
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

            if (xx >= cellsAcross - cellsDown - Y_MARGIN + yy)
            {
                return CellStatus.OffRhombus;        // right slope of rhombus
            }

            var alignment = yy % 2;
            return xx % 2 == alignment ? CellStatus.Free : CellStatus.Separator;
            
        }

        /// <summary>
        /// x-dim accounts for separators (accross * 2 - 1), the right shift (down - 1) and end guards (+4)  
        /// </summary>
        private (int, int) CalculateBoardSize(int gameCellsAcross, int gameCellsDown)
        {
            return (gameCellsAcross * 2 - 1 + gameCellsDown - 1 + X_MARGIN, gameCellsDown + Y_MARGIN * 2);
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
            for (int ii = 0; ii <= board.GetUpperBound(1); ii++)
            {
                for (int jj = 0; jj <= board.GetUpperBound(0); jj++)
                {
                    sb.Append((char)board[jj, ii].Status);
                }

                sb.Append('\n');
            }

            return sb.ToString();
        }
    }
    
    
    public Connect(string[] input)
    {
        MakeBoard(input);
    }

    private void MakeBoard(string[] input)
    {
        Game game = new Game(Side.Top, Side.Left, 2, 2);
        var xxx = game.ToString();
    }

    public ConnectWinner Result()
    {
        throw new NotImplementedException("You need to implement this function.");
    }
}