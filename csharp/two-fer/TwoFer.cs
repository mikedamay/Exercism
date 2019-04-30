public static class TwoFer
{
  public static string Name(string name = "")
  {
    if (name == "")
    {
      return "One for you, one for me.";
    }
    else
    {
      return $"One for {name}, one for me.";
    }
  }
}