
using System;
using Microsoft.VisualBasic.CompilerServices;

public static class TwoFer
{
  public static string Name(string input = "you")
  {
    return $"One for {input}, one for me.";
  }

  class MyClass
  {
    public MyClass(int val)
    {
      this.val = val;
    }
    private int val;

    public override bool Equals(object other)
    {
      return (other as MyClass).val == val;
    }

    protected bool Equals(MyClass other)
    {
      return other.val == val;
    }
    public static bool operator==(MyClass m1, MyClass m2)
    {
      return m1.Equals(m2);
    }
    public static bool operator!=(MyClass m1, MyClass m2)
    {
      return !m1.Equals(m2);
    }
  }
  public static bool AreEqual()
  {
     var m1 = new MyClass(42);
     var m2 = new MyClass(val: 42);
     return (object)m1 == (object)m2;
//     return m1.Equals(m2);
  }
  
}

namespace MyTests
{
  using Xunit;
  
  public class Tests
  {
    [Fact]
    public void TestOne()
    {
      Assert.True(TwoFer.AreEqual());    
    }
  }
}

/*
An effective solution
 
There are a few idioms could be useful for you to know about:
* null coalescence operator `var expr = "something or nothing"; var default_val = "nothing"; var foo = expr ?? default_val;`
* string interpolation `var expr = "abc"; var foo = $"this is {expr}";`
* default parameter can be something other than null - `void foo(string bar = "you"){}`

Note that only certain combinations of the above will make sense for this exercise.

You can research these if you choose and modify the solution as seems appropriate or 
leave as is and I will sign off.

*/  

/*
There are a few idioms you should be aware of:
* null coalescence operator `var expr = "something or nothing"; var default_val = "nothing"; var foo = expr ?? default_val;`
* default parameter can be something other than null - `void foo(string bar = "you"){}`

Note that using both of the above will not make sense for this exercise.  Use at most one.

You can research these if you choose and modify the solution as seems appropriate or 
leave as is and I will sign off.

*/