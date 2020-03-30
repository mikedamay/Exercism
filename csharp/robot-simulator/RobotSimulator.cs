using System;
using System.Collections.Generic;
using System.Linq;


public enum Direction
{
    North = 0,
    East = 1,
    South = 2,
    West = 3
}



public class RobotSimulator
{
    private struct State
    {
        public Direction Direction { get; set; }
        public int X { get; set; }
        public int Y { get; set; }
        public State(Direction direction, int x, int y)
        {
            Direction = direction;
            X = x;
            Y = y;
        }
    }

    public Direction Direction => state.Direction;
    public int X => state.X;
    public int Y => state.Y;
    
    private State state;
    public RobotSimulator(Direction direction, int x, int y)
    {
        state = new State(direction, x, y);
    }



    public void Move(string instructions) => instructions
        .Select<char, Action>(instruction => instruction switch
            {
            'L' => TurnLeft,
            'R' => TurnRight,
            'A' => Advance,
            _ => throw new ArgumentException()
            })
        .ToList()
        .ForEach(operation => operation.Invoke());


    private void Advance() =>
        (state.X, state.Y) = state.Direction switch
            {
            Direction.North => (state.X, state.Y + 1),
            Direction.East => (state.X + 1, state.Y),
            Direction.South => (state.X, state.Y - 1),
            Direction.West => (state.X - 1, state.Y),
            _ => throw new ArgumentOutOfRangeException()
            };


    private void TurnLeft() => state.Direction = (Direction) (((int) state.Direction + 3) % 4);
    private void TurnRight() => state.Direction = (Direction) (((int) state.Direction + 1) % 4);
}