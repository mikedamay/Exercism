using System.IO;

namespace ExerciseReport
{
    public interface IExerciseFileHandler
    {
        ExerciseObjectTree ReadFile();
        void WriteFile(ExerciseObjectTree exerciseObjectTree);

    }

    internal class ExerciseFileHandler
    {
        private readonly string trackConfigPathAndFileName;
        private readonly ExerciseJsonParser exerciseJsonParser;

        public ExerciseFileHandler(string root, string track, ExerciseJsonParser jsonParser)
        {
            trackConfigPathAndFileName = Path.Combine(root, "languages", track, "reference/exercises.json");
            exerciseJsonParser = jsonParser;
        }

        public ExerciseObjectTree ReadFile()
        {
            var text = File.ReadAllText(trackConfigPathAndFileName);
            return exerciseJsonParser.FromString(text);
        }

        public void WriteFile(ExerciseObjectTree exerciseObjectTree)
        {
            var text = exerciseJsonParser.ToString(exerciseObjectTree);
            File.WriteAllText(trackConfigPathAndFileName, text);
        }
    }
}