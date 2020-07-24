using System.Collections.Generic;
using System.Linq;

namespace ExerciseReport
{
    internal class ExerciseMerger
    {
        private readonly ExerciseFileHandler exerciseFileHandler;
        private readonly DesignDocCollator designDocCollator;
        private readonly string track;

        private const string TestRoot = "/Users/mikedamay/projects/exercism/v3";
        private const string CSharpTrack = "csharp";

        public static ExerciseMerger TestCSharpMerger { get; } =
            new ExerciseMerger(CSharpTrack, new ExerciseFileHandler(TestRoot, CSharpTrack,
                    new ExerciseFileJsonHandler())
                , new DesignDocCollator(TestRoot, new DesignDocParser()
                    , new DesignDocFileHandler(TestRoot)));

        public ExerciseMerger(string track,
            ExerciseFileHandler exerciseFileHandler, DesignDocCollator designDocCollator)
        {
            this.exerciseFileHandler = exerciseFileHandler;
            this.designDocCollator = designDocCollator;
            this.track = track;
        }

        public void Merge()
        {
            var exerciseFile = MergeLearningObjectives();
            exerciseFileHandler.WriteFile(exerciseFile);
        }

        public ExerciseFile MergeLearningObjectives()
        {
            var exerciseFile = exerciseFileHandler.ReadFile();
            var learningObjectives = designDocCollator.GetLearningObjectives(track);
            MergeLearningObjectives(exerciseFile, learningObjectives.learningObjectives);
            return exerciseFile;
        }

        private void MergeLearningObjectives(ExerciseFile exerciseFile, LearningObjectives learningObjectives)
        {
            var concepts = exerciseFile.Exercises.SelectMany(ex => ex.Concepts);
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