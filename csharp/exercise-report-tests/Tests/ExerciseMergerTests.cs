using Xunit;
using static ExerciseReport.Tests.Utils;

namespace ExerciseReport.Tests
{
    public class ExerciseMergerTests
    {
        [Fact]
        public void ParseDesignDoc_WithMultipleHashes_ReportsNoErrors()
        {
            (var merger, var exerciseResourceHandler) = Utils.GetMergerFromResourcesPlusHandler(
                Constants.ExercisesMultipleObjectivesResource,
                Constants.DesignMultipleHashesResource
            );
            merger.MergeInLearningObjectives();
            Assert.Contains("\"know how to use string interpolation on values of any type\"",
                exerciseResourceHandler.ExerciseResultJson);
        }

        [Fact]
        public void Merge_LearningObjectivesWithNoMatchingConcepts_ReportsErrors()
        {
            (var merger, var exerciseResourceHandler) = Utils.GetMergerFromResourcesPlusHandler(
                Constants.ExercisesOrphanedConceptsResource,
                Constants.DesignOrphanedConceptsResource
            );
            merger.MergeInLearningObjectives();
            Assert.NotEmpty(exerciseResourceHandler.ErrorResultJson);
            Assert.NotEqual("{\n  \"Errors\": []\n}", exerciseResourceHandler.ErrorResultJson);
        }

        [Fact]
        public void Merge_LearningObjectivesWithMatchingConcepts_ReportsNoErrors()
        {
            (var merger, var exerciseResourceHandler) = Utils.GetMergerFromResourcesPlusHandler(
                Constants.ExercisesUnorphanedConceptsResource,
                Constants.DesignUnorphanedConceptsResource
            );
            merger.MergeInLearningObjectives();
            Assert.Equal("{\n  \"Errors\": []\n}", exerciseResourceHandler.ErrorResultJson);
        }

        [Fact]
        public void Merge_ConceptsWithNoObjectives_ReportsErrors()
        {
            (var merger, var exerciseResourceHandler) = Utils.GetMergerFromResourcesPlusHandler(
                Constants.ExercisesNoObjectivesResource,
                Constants.DesignEmptyResource
            );
            merger.MergeInLearningObjectives();
            Assert.Contains("The string-formatting concept has no learning objectives",
                exerciseResourceHandler.ErrorResultJson);
        }

        [Fact]
        public void Merge_ConceptsWithObjectives_ReportsNoErrors()
        {
            (var merger, var exerciseResourceHandler) = Utils.GetMergerFromResourcesPlusHandler(
                Constants.ExercisesNoObjectivesResource,
                Constants.DesignJustConceptsResource
            );
            merger.MergeInLearningObjectives();
            Assert.Equal("{\n  \"Errors\": []\n}", exerciseResourceHandler.ErrorResultJson);
        }

        [Fact]
        public void Merge_WithTooManyErrors_DoesNotWriteExerciseReport()
        {
            var exerciseResourceHandler
                = new ExerciseResourceHandler(Constants.ExercisesNoObjectivesResource);
            var merger =
                new ExerciseMerger(new ExerciseFileCollator(
                        exerciseResourceHandler,
                        new ExerciseJsonParser())
                    , new DesignDocCollator(
                        new DesignDocResourceHandler(Constants.DesignEmptyResource),
                        new DesignDocParser()),
                    maxErrors: 1
                );

            merger.MergeInLearningObjectives();
            Assert.Empty(exerciseResourceHandler.ExerciseResultJson);
            Assert.Contains("Too many", exerciseResourceHandler.ErrorResultJson);
        }
    }
}