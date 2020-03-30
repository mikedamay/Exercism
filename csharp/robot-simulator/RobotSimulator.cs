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

internal struct State
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

    public Direction Direction => finalState.Direction;
    public int X => finalState.X;
    public int Y => finalState.Y;
    
    private readonly State initialState;
    private State finalState;
    public RobotSimulator(Direction direction, int x, int y)
    {
        initialState = new State(direction, x, y);
        finalState = initialState;
    }
    
    public void Move(string instructions)
    {
        finalState = instructions
            .Select<char, Func<State, State>>(instruction => instruction switch
                {
                'L' => TurnLeftMove,
                'R' => TurnRightMove,
                'A' => AdvanceMove,
                _ => throw new ArgumentException()
                })
            .MoveFromStateToState(initialState).Last();
    }
    
    private State AdvanceMove(State state) =>
        new State(state.Direction, state.Direction switch
            {
            Direction.North => (state.X, state.Y + 1),
            Direction.East => (state.X + 1, state.Y),
            Direction.South => (state.X, state.Y - 1),
            Direction.West => (state.X - 1, state.Y),
            _ => throw new ArgumentOutOfRangeException()
            });
    
    private State TurnLeftMove(State state) => new State((Direction) (((int) state.Direction + 3) % 4), state.X, state.Y);
    private State TurnRightMove(State state) => new State((Direction) (((int) state.Direction + 1) % 4), state.X, state.Y);
}

internal static class RobotSimulatorExtensions
{
    public static IEnumerable<State> MoveFromStateToState(this IEnumerable< Func<State, State>> moves, State initialState)
    {
        State st = initialState;
        foreach (var move in moves)
        {
            st = move(st);
            yield return st;
        }
    }
}