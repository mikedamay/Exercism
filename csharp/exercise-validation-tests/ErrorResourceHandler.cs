using ExerciseReport;

namespace ExerciseValidationTests
{
    internal class ErrorResourceHandler : IErrorFileHandler
    {
        public string ResultJson { get; private set; } = string.Empty;

        public string ReadFile()
        {
            return Utils.GetResourceAsString(Constants.ErrorsResource);
        }

        public void WriteFile(string errorJson)
        {
            ResultJson = errorJson;
        }
    }
}