using System;
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
            merger.MergeInLearningObjectives();
            var reporter = ReportCollator.CSharpReportCollator;
            var efc = ExerciseFileCollator.CSharpExerciseFileCollator;
            var result = efc.ReadExercises();
            if (result.result == Result.FatalError)
            {
                throw new Exception("Failed to produce report");
            }
            reporter.WriteReport(result.exerciseObjectTree);
        }
    }
}