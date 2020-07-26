using System.Collections.Generic;
using System.Linq;
using ExerciseReport.Tests;

namespace ExerciseReport
{
    internal class ExerciseMerger
    {
        private readonly ExerciseFileCollator exerciseFileHandler;
        private readonly DesignDocCollator designDocCollator;
        private readonly string track;

        private const string TestRoot = Constants.TestUserRoot;
        private const string CSharpTrack = Constants.CSharpTrack;

        public static ExerciseMerger TestCSharpMerger { get; } =
            new ExerciseMerger(CSharpTrack, new ExerciseFileCollator(new ExerciseResourceHandler(), 
                    new ExerciseJsonParser())
                , new DesignDocCollator(TestRoot, new DesignDocParser()
                    , new DesignDocFileHandler(TestRoot, Constants.CSharpTrack)));

        public ExerciseMerger(string track,
            ExerciseFileCollator exerciseFileHandler, DesignDocCollator designDocCollator)
        {
            this.exerciseFileHandler = exerciseFileHandler;
            this.designDocCollator = designDocCollator;
            this.track = track;
        }

        public void Merge()
        {
            var outputs = MergeLearningObjectives();
            exerciseFileHandler.WriteExercises(outputs.exerciseObjectTree);
        }

        public (Result result, ExerciseObjectTree exerciseObjectTree, List<Error> errors) MergeLearningObjectives()
        {
            var outputs = exerciseFileHandler.ReadExercises();
            if (outputs.result == Result.FatalError)
            {
                return outputs;
            }
            var learningObjectives = designDocCollator.GetLearningObjectives(track);
            MergeLearningObjectives(outputs.exerciseObjectTree, learningObjectives.learningObjectives);
            return (Result.FatalError, outputs.exerciseObjectTree, outputs.errors);
        }

        private void MergeLearningObjectives(ExerciseObjectTree exerciseObjectTree, LearningObjectives learningObjectives)
        {
            var concepts = exerciseObjectTree.Exercises.SelectMany(ex => ex.Concepts);
            foreach (Concept concept in concepts)
            {
                var objectives = learningObjectives.GetList(concept.Name);
                if (objectives != null)
                {
                    foreach (string objective in objectives)
                    {
                        concept.LearningObjectives.Add(objective);
                    }
                }
            }
        }
    }
}