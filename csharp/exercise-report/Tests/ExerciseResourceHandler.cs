using System;
using System.IO;

namespace ExerciseReport.Tests
{
    public class ExerciseResourceHandler : IExerciseFileHandler
    {
        public string ResultJson { get; private set; }

        public string ReadFile()
        {
            return ExerciseReportTests.GetResourceAsString(Constants.ExercisesResource);
        }

        public void WriteFile(string exerciseJson)
        {
            ResultJson = exerciseJson;
        }
    }
}