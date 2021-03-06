using ExerciseReport;

namespace ExerciseValidationTests
{
    internal class ExerciseResourceHandler : IExerciseFileHandler
    {
        public string ExerciseResultJson { get; private set; } = string.Empty;

        private readonly string exerciseResourceName;

        public ExerciseResourceHandler(string exerciseResourceName)
        {
            this.exerciseResourceName = exerciseResourceName;
        }
        
        public string ReadFile()
        {
            return Utils.GetResourceAsString(exerciseResourceName);
        }

        public void WriteFile(string exerciseJson)
        {
            ExerciseResultJson = exerciseJson;
        }
    }
}