using System.Collections.Generic;
using System.Linq;

namespace ExerciseReport
{
    internal class ExerciseMerger
    {
        private readonly ExerciseFileHandler exerciseFileHandler;
        private readonly DesignDocCollator designDocCollator;
        private readonly string track;
        
        public ExerciseMerger(string track,
            ExerciseFileHandler exerciseFileHandler, DesignDocCollator designDocCollator)
        {
            this.exerciseFileHandler = exerciseFileHandler;
            this.designDocCollator = designDocCollator;
            this.track = track;
        }

        public void MergeLearningObjectives()
        {
            var exerciseFile = exerciseFileHandler.ReadFile();
            var learningObjectives = designDocCollator.GetLearningObjectives(track);
            MergeLearningObjectives(exerciseFile, learningObjectives.learningObjectives);
            exerciseFileHandler.WriteFile(exerciseFile);
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