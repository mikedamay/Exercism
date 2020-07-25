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

        public ExerciseObjectTree ReadFile()
        {
            var text = File.ReadAllText(trackConfigPathAndFileName);
            return exerciseFileJsonHandler.FromString(text);
        }

        public void WriteFile(ExerciseObjectTree exerciseObjectTree)
        {
            var text = exerciseFileJsonHandler.ToString(exerciseObjectTree);
            File.WriteAllText(trackConfigPathAndFileName, text);
        }
    }
}