// This file was auto-generated based on version 2.1.0 of the canonical data.

using System;
using Xunit;

public class DominoesTest
{
    [Fact]
    public void multiple()
    {
        for (int ii = 0; ii < 100_000; ii++)
        {
            Empty_input_empty_output();
            Singleton_input_singleton_output();
            Singleton_that_cant_be_chained();
            Three_elements();
            Can_reverse_dominoes();
            Cant_be_chained();
            Disconnected_simple();
            Disconnected_double_loop();
            Disconnected_single_isolated();
            Need_backtrack();
            Separate_loops();
            Nine_elements();
        }
    }
    [Fact]
    public void Empty_input_empty_output()
    {
        var dominoes = Array.Empty<(int, int)>();
        Assert.True(Dominoes.CanChain(dominoes));
    }

    [Fact(Skip = "")]
    public void Singleton_input_singleton_output()
    {
        var dominoes = new[] { (1, 1) };
        Assert.True(Dominoes.CanChain(dominoes));
    }

    [Fact(Skip = "")]
    public void Singleton_that_cant_be_chained()
    {
        var dominoes = new[] { (1, 2) };
        Assert.False(Dominoes.CanChain(dominoes));
    }

    [Fact(Skip = "")]
    public void Three_elements()
    {
        var dominoes = new[] { (1, 2), (3, 1), (2, 3) };
        Assert.True(Dominoes.CanChain(dominoes));
    }

    [Fact(Skip = "")]
    public void Can_reverse_dominoes()
    {
        var dominoes = new[] { (1, 2), (1, 3), (2, 3) };
        Assert.True(Dominoes.CanChain(dominoes));
    }

    [Fact(Skip = "")]
    public void Cant_be_chained()
    {
        var dominoes = new[] { (1, 2), (4, 1), (2, 3) };
        Assert.False(Dominoes.CanChain(dominoes));
    }

    [Fact(Skip = "")]
    public void Disconnected_simple()
    {
        var dominoes = new[] { (1, 1), (2, 2) };
        Assert.False(Dominoes.CanChain(dominoes));
    }

    [Fact(Skip = "")]
    public void Disconnected_double_loop()
    {
        var dominoes = new[] { (1, 2), (2, 1), (3, 4), (4, 3) };
        Assert.False(Dominoes.CanChain(dominoes));
    }

    [Fact(Skip = "")]
    public void Disconnected_single_isolated()
    {
        var dominoes = new[] { (1, 2), (2, 3), (3, 1), (4, 4) };
        Assert.False(Dominoes.CanChain(dominoes));
    }

    [Fact(Skip = "")]
    public void Need_backtrack()
    {
        var dominoes = new[] { (1, 2), (2, 3), (3, 1), (2, 4), (2, 4) };
        Assert.True(Dominoes.CanChain(dominoes));
    }

    [Fact(Skip = "")]
    public void Need_backtrack_modified()
    {
        var dominoes = new[] { (1, 2), (3, 1), (2, 4), (2, 4), (2, 3) };
        Assert.True(Dominoes.CanChain(dominoes));
    }
    
    
    [Fact(Skip = "")]
    public void Separate_loops()
    {
        var dominoes = new[] { (1, 2), (2, 3), (3, 1), (1, 1), (2, 2), (3, 3) };
        Assert.True(Dominoes.CanChain(dominoes));
    }

    [Fact(Skip = "")]
    public void Nine_elements()
    {
        var dominoes = new[] { (1, 2), (5, 3), (3, 1), (1, 2), (2, 4), (1, 6), (2, 3), (3, 4), (5, 6) };
        Assert.True(Dominoes.CanChain(dominoes));
    }
}