using System;
using System.Collections.Generic;
using System.Linq;

/**
 * based on a idea by David Godfrey
 */

public enum Direction
{
    North = 0,
    East = 1,
    South = 2,
    West = 3
}

public struct State
{
    public Direction Direction { get;}
    public int X { get;}
    public int Y { get;}
    public State(Direction direction, (int x, int y) coords)
    {
        Direction = direction;
        X = coords.x;
        Y = coords.y;
    }

    public State(Direction direction, int x, int y) : this(direction, (x, y)) { }
}



public class RobotSimulator
{

    public Direction Direction => state.Direction;
    public int X => state.X;
    public int Y => state.Y;
    
    private State state;
    public RobotSimulator(Direction direction, int x, int y)
    {
        state = new State(direction, x, y);
    }



    public void Move(string instructions)
    {
        var aaa = instructions
            .Select<char, Func<State, State>>(instruction => instruction switch
                {
                'L' => TurnLeft,
                'R' => TurnRight,
                'A' => Advance,
                _ => throw new ArgumentException()
                })
            .ProcessMove(state).Last();
    }

    
    private void Advance(State state) =>
        new State(state.Direction, state.Direction switch
            {
            Direction.North => (state.X, state.Y + 1),
            Direction.East => (state.X + 1, state.Y),
            Direction.South => (state.X, state.Y - 1),
            Direction.West => (state.X - 1, state.Y),
            _ => throw new ArgumentOutOfRangeException()
            });


    private State TurnLeft(State state) => new State((Direction) (((int) state.Direction + 3) % 4), state.X, state.Y);
    private State TurnRight(State state) => new State((Direction) (((int) state.Direction + 1) % 4), state.X, state.Y);
}

internal static class RobotSimulatorExtensions
{
    public static IEnumerable<State> ProcessMove(this IEnumerable< Func<State, State>> moves, State initialState)
    {
        State st = initialState;
        foreach (var move in moves)
        {
            yield return move(st);
        }
    }
}