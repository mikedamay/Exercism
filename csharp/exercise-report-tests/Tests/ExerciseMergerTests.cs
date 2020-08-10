using System.Linq;
using Xunit;

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
            Assert.Contains("\"know how to use string interpolation on values of any type\"", exerciseResourceHandler.ExerciseResultJson);
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
            Assert.NotEqual("{\n  \"Errors\": []\n}",exerciseResourceHandler.ErrorResultJson);
        }

        [Fact]
        public void Merge_LearningObjectivesWithMatchingConcepts_ReportsNoErrors()
        {
            (var merger, var exerciseResourceHandler) = Utils.GetMergerFromResourcesPlusHandler(
                Constants.ExercisesUnorphanedConceptsResource,
                Constants.DesignUnorphanedConceptsResource
            );
            merger.MergeInLearningObjectives();
            Assert.Equal("{\n  \"Errors\": []\n}",exerciseResourceHandler.ErrorResultJson);
        }
    }
}