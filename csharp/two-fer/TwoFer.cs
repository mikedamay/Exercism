
public static class TwoFer
{
  public static string Name(string input = "you")
  {
    return $"One for {input}, one for me.";
  }
}
/*
There are a few idioms you should be aware of:
* null coalescence operator `var expr = "something or nothing"; var default_val = "nothing"; var foo = expr ?? default_val;`
* string interpolation `var expr = "abc"; var foo = $"this is {expr}";`
* default parameter can be something other than null - `void foo(string bar = "you"){}`

Note that only certain combinations of the above will make sense for this exercise.

You can research these if you choose and modify the solution as seems appropriate or 
leave as is and I will sign off.

*/  