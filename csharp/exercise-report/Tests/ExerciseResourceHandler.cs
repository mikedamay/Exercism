using System;
using System.IO;

namespace ExerciseReport.Tests
{
    public class ExerciseResourceHandler : IExerciseFileHandler
    {
        public string ResultJson { get; private set; }

        public string ReadFile()
        {
            Stream? stream = this.GetType().Assembly.GetManifestResourceStream(Constants.ExercisesResource);
            string exerciseJson = string.Empty;
            if (stream != null)
            {
                using (stream)
                using (var reader = new StreamReader(stream))
                    exerciseJson = reader.ReadToEnd();
            }
            else
            {
                throw new NullReferenceException($"{nameof(stream)} is null - missing resource - {Constants.ExercisesResource}");
            }
            return exerciseJson;
        }

        public void WriteFile(string exerciseJson)
        {
            ResultJson = exerciseJson;
        }
    }
}