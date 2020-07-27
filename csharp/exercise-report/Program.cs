using System.IO;

namespace ExerciseReport
{
    public class Program
    {
        public static void Main(string[] args)
        {
            if (args.Length > 0)
            {
                Directory.SetCurrentDirectory(args[0]);
            }
            var merger = ExerciseMerger.CSharpMerger;
            merger.Merge();
        }
    }
}