using System;
using System.IO;

namespace ExerciseReport
{
    public class Program
    {
        public static int Main(string[] args)
        {
            try
            {
                // usage: dotnet run # in production no param is required
                // usage: dotnet run [/myProjects/exercism/v3]
                if (args.Length > 0)
                {
                    Directory.SetCurrentDirectory(args[0]);
                        // e.g. /Users/mikedamay/projects/exercism/v3
                }
                var merger = ExerciseMerger.CSharpMerger;
                var mergeResults = merger.MergeExercisesAndLearningObjectives();
                var errorWriter = ErrorWriter.CSharpErrorWriter;
                errorWriter.Write(mergeResults.Errors);
                var reporter = ReportReader.CSharpReportReader;
                if (mergeResults.Result == Result.FatalError)
                {
                    throw new Exception("Failed to produce report: " + mergeResults.Errors[^1].Message);
                }
                reporter.WriteReport(mergeResults.ExerciseObjectTree);
                return 0;
            }
            catch (Exception e)
            {
                Console.Out.WriteLine(e.Message);
                return 1;
            }
        }
    }
}