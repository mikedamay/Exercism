using System.IO;

namespace ExerciseReport
{
    public class ExerciseFileHandler
    {
        private readonly string trackConfigPathAndFileName;
        private readonly ExerciseJsonHandler exerciseJsonHandler;

        public ExerciseFileHandler(string root, string track, ExerciseJsonHandler jsonHandler)
        {
            trackConfigPathAndFileName = Path.Combine(root, "languages", track, "reference/exercises.json");
            exerciseJsonHandler = jsonHandler;
        }

        public ExerciseObjectTree ReadFile()
        {
            var text = File.ReadAllText(trackConfigPathAndFileName);
            return exerciseJsonHandler.FromString(text);
        }

        public void WriteFile(ExerciseObjectTree exerciseObjectTree)
        {
            var text = exerciseJsonHandler.ToString(exerciseObjectTree);
            File.WriteAllText(trackConfigPathAndFileName, text);
        }
    }
}