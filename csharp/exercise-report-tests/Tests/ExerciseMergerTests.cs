using Xunit;

namespace ExerciseReport.Tests
{
    public class ExerciseMergerTests
    {
        [Fact]
        public void ParseDesignDoc_WithMultipleHashes_ReportsNoErrors()
        {
           var merger = Utils.GetMergerFromResources(
                Constants.ExercisesMultipleObjectivesResource,
                Constants.DesignMultipleHashesResource
            );
            var mergeResults = merger.MergeExercisesAndLearningObjectives();
            Assert.Contains("know how to use string interpolation on values of any type",
                mergeResults.ExerciseObjectTree
                    .Exercises[0]
                    .Concepts[0]
                    .LearningObjectives[1]);
        }

        [Fact]
        public void Merge_LearningObjectivesWithNoMatchingConcepts_ReportsErrors()
        {
            var merger = Utils.GetMergerFromResources(
                Constants.ExercisesOrphanedConceptsResource,
                Constants.DesignOrphanedConceptsResource
            );
            var mergeResults = merger.MergeExercisesAndLearningObjectives();
            Assert.Equal(2, mergeResults.Errors.Count);
        }

        [Fact]
        public void Merge_LearningObjectivesWithMatchingConcepts_ReportsNoErrors()
        {
            var merger = Utils.GetMergerFromResources(
                Constants.ExercisesUnorphanedConceptsResource,
                Constants.DesignUnorphanedConceptsResource
            );
            var mergeResults = merger.MergeExercisesAndLearningObjectives();
            Assert.Empty(mergeResults.Errors);
        }

        [Fact]
        public void Merge_ConceptsWithNoObjectives_ReportsErrors()
        {
            var merger = Utils.GetMergerFromResources(
                Constants.ExercisesNoObjectivesResource,
                Constants.DesignMissingObjectiveResource
            );
            var mergeResults = merger.MergeExercisesAndLearningObjectives();
            Assert.Contains("The string-formatting concept has no learning objectives",
                mergeResults.Errors[0].Message);
        }

        [Fact]
        public void Merge_ConceptsWithObjectives_ReportsNoErrors()
        {
            var merger = Utils.GetMergerFromResources(
                Constants.ExercisesNoObjectivesResource,
                Constants.DesignJustConceptsResource
            );
            var mergeResults = merger.MergeExercisesAndLearningObjectives();
            Assert.Empty(mergeResults.Errors);
        }

        [Fact]
        public void Merge_WithTooManyErrors_DoesNotWriteExerciseReport()
        {
            var exerciseResourceHandler
                = new ExerciseResourceHandler(Constants.ExercisesNoObjectivesResource);
            var merger =
                new ExerciseMerger(new ExerciseReader(
                        exerciseResourceHandler,
                        new ExerciseJsonParser())
                    , new DesignDocReader(
                        new DesignDocResourceHandler(Constants.DesignEmptyResource),
                        new DesignDocParser()),
                    maxErrors: 1
                );

            var mergeResults = merger.MergeExercisesAndLearningObjectives();
            Assert.Equal(Result.FatalError, mergeResults.Result);
        }
    }
}