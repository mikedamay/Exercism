using System;
using System.IO;
using ExerciseReport;

namespace ExerciseValidation
{
    class Program
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
                var comparer = ExerciseComparer.CSharpComparer;
                var errorWriter = ErrorWriter.CSharpErrorWriter;
                var reporter = ReportWriter.CSharpReportWriter;
                var mergeResults = comparer.CompareExercises();
                errorWriter.Write(mergeResults.Errors);
            
                if (mergeResults.Result == Result.FatalError)
                {
                    throw new Exception("Failed to produce report: " + mergeResults.Errors[^1].Message);
                }

                reporter.WriteReport(mergeResults.NotInExerciseReport, mergeResults.NotInTrackConfig);
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