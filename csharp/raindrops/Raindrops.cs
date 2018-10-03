using System.Linq;
 
 public static class Raindrops
 {
     public static string Convert(int number)
       => new (string sound, int value)[] {("Pling", 3), ("Plang", 5), ("Plong", 7),}
             .Where(p => number % p.value == 0).Select(p => p.sound)
             .DefaultIfEmpty(number.ToString()).Aggregate((a, b) => a + b);
 }