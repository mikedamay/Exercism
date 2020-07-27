using System;
using System.IO;
using FSharp.Compiler;

namespace ExerciseReport.Tests
{
    internal class ExerciseResourceHandler : IExerciseFileHandler
    {
        public string ExerciseResultJson { get; private set; } = string.Empty;
        public string ErrorResultJson { get; private set; } = string.Empty;

        private readonly string exerciseResourceName;

        public ExerciseResourceHandler(string exerciseResourceName = Constants.ExercisesResource)
        {
            this.exerciseResourceName = exerciseResourceName;
        }
        
        public string ReadExerciseFile()
        {
            return Utils.GetResourceAsString(exerciseResourceName);
        }

        public void WriteExerciseFile(string exerciseJson)
        {
            ExerciseResultJson = exerciseJson;
        }

        public string ReadErrorFile()
        {
            return Utils.GetResourceAsString(Constants.ErrorsResource);
        }

        public void WriteErrorFile(string errorJson)
        {
            ErrorResultJson = errorJson;
        }
    }
}