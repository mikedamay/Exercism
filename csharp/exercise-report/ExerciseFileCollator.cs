using System;
using System.Collections.Generic;

namespace ExerciseReport
{
    internal class ExerciseFileCollator
    {
        private readonly IExerciseFileHandler exerciseFileHandler;
        private readonly ExerciseJsonParser exerciseJsonParser;

        public ExerciseFileCollator(IExerciseFileHandler exerciseFileHandler,
            ExerciseJsonParser exerciseJsonParser)
        {
            this.exerciseFileHandler = exerciseFileHandler;
            this.exerciseJsonParser = exerciseJsonParser;
        }

        public (Result result, ExerciseObjectTree exerciseObjectTree, List<Error> errors) 
            ReadExercises()
        {
            try
            {
                var text = exerciseFileHandler.ReadFile();
                return exerciseJsonParser.FromString(text);
            }
            catch (Exception e)
            {
                return (
                    Result.FatalError,
                    new ExerciseObjectTree(),
                    new List<Error>{new Error(Severity.Fatal, "reading exercise.json file: " + e.Message)}
                    );
            }
        }

        public (Result result, List<Error> errors) 
            WriteExercises(ExerciseObjectTree exerciseObjectTree)
        {
            try
            {
                var text = exerciseJsonParser.ToString(exerciseObjectTree);
                exerciseFileHandler.WriteFile(text);
                return (Result.Success, new List<Error>());
            }
            catch (Exception e)
            {
                return (
                    Result.FatalError,
                    new List<Error>{new Error(Severity.Fatal, "writing exercise.json file: " + e.Message)}
                );
            }
        }
    }
}