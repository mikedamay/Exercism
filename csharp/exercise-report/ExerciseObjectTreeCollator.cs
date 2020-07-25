namespace ExerciseReport
{
    internal class ExerciseObjectTreeCollator
    {
        private IExerciseFileHandler exerciseFileHandler;
        private ExerciseJsonParser exerciseJsonParser;

        public ExerciseObjectTreeCollator(IExerciseFileHandler exerciseFileHandler,
            ExerciseJsonParser exerciseJsonParser)
        {
            this.exerciseFileHandler = exerciseFileHandler;
            this.exerciseJsonParser = exerciseJsonParser;
        }

        public ExerciseObjectTree ReadExercises()
        {
            var text = exerciseFileHandler.ReadFile();
            return exerciseJsonParser.FromString(text);
        }

        public void WriteExercises(ExerciseObjectTree exerciseObjectTree)
        {
            var text = exerciseJsonParser.ToString(exerciseObjectTree);
            exerciseFileHandler.WriteFile(text);
        }
    }
}