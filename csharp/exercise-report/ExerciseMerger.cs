using System.Collections.Generic;
using System.Linq;

namespace ExerciseReport
{
    internal class ExerciseMerger
    {
        private readonly ExerciseObjectTreeCollator exerciseFileHandler;
        private readonly DesignDocCollator designDocCollator;
        private readonly string track;

        private const string TestRoot = "/Users/mikedamay/projects/exercism/v3";
        private const string CSharpTrack = "csharp";

        public static ExerciseMerger TestCSharpMerger { get; } =
            new ExerciseMerger(CSharpTrack, new ExerciseObjectTreeCollator(new ExerciseFileHandler( TestRoot, CSharpTrack),
                    new ExerciseJsonParser())
                , new DesignDocCollator(TestRoot, new DesignDocParser()
                    , new DesignDocFileHandler(TestRoot, "csharp")));

        public ExerciseMerger(string track,
            ExerciseObjectTreeCollator exerciseFileHandler, DesignDocCollator designDocCollator)
        {
            this.exerciseFileHandler = exerciseFileHandler;
            this.designDocCollator = designDocCollator;
            this.track = track;
        }

        public void Merge()
        {
            var exerciseFile = MergeLearningObjectives();
            exerciseFileHandler.WriteExercises(exerciseFile);
        }

        public ExerciseObjectTree MergeLearningObjectives()
        {
            var exerciseFile = exerciseFileHandler.ReadExercises();
            var learningObjectives = designDocCollator.GetLearningObjectives(track);
            MergeLearningObjectives(exerciseFile, learningObjectives.learningObjectives);
            return exerciseFile;
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