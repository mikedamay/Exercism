using System.IO;

namespace ExerciseReport
{
    public class ExerciseFileHandler
    {
        private readonly string trackConfigPathAndFileName;
        private readonly ExerciseFileJsonHandler exerciseFileJsonHandler;

        public ExerciseFileHandler(string root, string track, ExerciseFileJsonHandler jsonHandler)
        {
            trackConfigPathAndFileName = Path.Combine(root, "languages", track, "reference/exercises.json");
            exerciseFileJsonHandler = jsonHandler;
        }

        public ExerciseFile ReadFile()
        {
            var text = File.ReadAllText(trackConfigPathAndFileName);
            return exerciseFileJsonHandler.FromString(text);
        }

        public void WriteFile(ExerciseFile exerciseFile)
        {
            var text = exerciseFileJsonHandler.ToString(exerciseFile);
            File.WriteAllText(trackConfigPathAndFileName, text);
        }
    }
}