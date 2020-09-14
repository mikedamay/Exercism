using ExerciseValidation;

namespace ExerciseValidationTests
{
    internal class TrackConfigResourceHandler : ITrackConfigFileHandler
    {
        public string ExerciseResultJson { get; private set; } = string.Empty;

        private readonly string exerciseResourceName;

        public TrackConfigResourceHandler(string exerciseResourceName)
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