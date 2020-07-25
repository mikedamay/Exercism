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
    }
}