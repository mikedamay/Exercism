using System.IO;

namespace ExerciseReport
{
    public interface IExerciseFileHandler
    {
        string ReadFile();
        void WriteFile(string exerciseJson);

    }

    internal class ExerciseFileHandler : IExerciseFileHandler
    {
        private readonly string trackConfigPathAndFileName;

        public ExerciseFileHandler(string root, string track)
        {
            trackConfigPathAndFileName = Path.Combine(root, "languages", track, "reference/exercises.json");
        }

        public string ReadFile()
        {
            return File.ReadAllText(trackConfigPathAndFileName);
        }

        public void WriteFile(string exerciseJson)
        {
            File.WriteAllText(trackConfigPathAndFileName, exerciseJson);
        }
    }
}