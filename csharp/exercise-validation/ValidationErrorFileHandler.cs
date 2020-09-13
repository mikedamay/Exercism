using System.IO;
using ExerciseReport;

namespace ExerciseValidation
{
    internal class ValidationErrorFileHandler : IErrorFileHandler
    {
        private readonly string pathAndFileName;

        public ValidationErrorFileHandler(string root, string track)
        {
            pathAndFileName = Path.Combine(
                root,
                PathNames.Default.Languages,
                track,
                Constants.ValidtionErrorFile);
        }

        public string ReadFile()
        {
            return File.ReadAllText(pathAndFileName);
        }

        public void WriteFile(string errorJson)
        {
            File.WriteAllText(pathAndFileName, errorJson);
        }
    }
}