﻿using System;
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

internal delegate State RobotAction(State st);


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
    
    public void Move(string instructionLetters)
    {
        finalState = instructionLetters
            .Select<char, RobotAction>(letter => letter switch
                {
                    'L' => TurnLeftAction,
                    'R' => TurnRightAction,
                    'A' => MoveForwardAction,
                    _ => throw new ArgumentException()
                }
            )
            .ExecuteRobotActions(initialState).Last();
    }
    
    private State MoveForwardAction(State state) =>
        new State(state.Direction
            , state.Direction switch
                {
                    Direction.North => (state.X, state.Y + 1),
                    Direction.East => (state.X + 1, state.Y),
                    Direction.South => (state.X, state.Y - 1),
                    Direction.West => (state.X - 1, state.Y),
                    _ => throw new ArgumentOutOfRangeException()
                }
            );
    
    private State TurnLeftAction(State state) => new State((Direction) (((int) state.Direction + 3) % 4), state.X, state.Y);
    private State TurnRightAction(State state) => new State((Direction) (((int) state.Direction + 1) % 4), state.X, state.Y);
}

internal static class RobotSimulatorExtensions
{
    public static IEnumerable<State> ExecuteRobotActions(this IEnumerable<RobotAction> robotActions, State initialState)
    {
        State state = initialState;
        foreach (var robotAction in robotActions)
        {
            state = robotAction(state);
            yield return state;
        }
    }
}