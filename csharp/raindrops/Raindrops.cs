using System.Linq;
 
 public static class Raindrops
 {
     public static string Convert(int number)
       => new (string sound, int value)[] {("Pling", 3), ("Plang", 5), ("Plong", 7),}
             .Where(p => number % p.value == 0).Select(p => p.sound)
             .DefaultIfEmpty(number.ToString()).Aggregate((a, b) => a + b);
 }
 /*
// successes
if (true) Console.WriteLine("success");
if (true == true) Console.WriteLine("success");
if (2+2 == 4) Console.WriteLine("success");
if (2+2 == 4 == true) Console.WriteLine("success");
if (false == false) Console.WriteLine("success");
var bob1 = 5; if (bob1 > 4) Console.WriteLine("success");
var bob2 = 5; if (bob2 > 4 == true) Console.WriteLine("success");
if (2+2 != 4 == false) Console.WriteLine("success");
// failures
var bob3 = 5; if (bob3 > 4 == false) var _; else Console.WriteLine("failure");
if (false) var _; else Console.WriteLine("failure");
if (2+2 != 4) var _; else Console.WriteLine("failure");
if (2+2 != 4 == true) var _; else Console.WriteLine("failure");
*/