using System;
using System.IO;
using FSharp.Compiler;

namespace ExerciseReport.Tests
{
    internal class ExerciseResourceHandler : IExerciseFileHandler
    {
        public string ResultJson { get; private set; } = string.Empty;

        private readonly string resourceName;

        public ExerciseResourceHandler(string resourceName = Constants.ExercisesResource)
        {
            this.resourceName = resourceName;
        }
        
        public string ReadFile()
        {
            return Utils.GetResourceAsString(resourceName);
        }

        public void WriteFile(string exerciseJson)
        {
            ResultJson = exerciseJson;
        }
    }
}