using Xunit;

namespace ExerciseReport.Tests
{
    public class ExerciseMergerTests
    {
        [Fact]
        public void Merge_LearningObjectivesWithNoMatchingConcepts_ReportsErrors()
        {
            var exerciseResourceHandler = new ExerciseResourceHandler(
                Constants.ExercisesOrphanedConceptsResource);
            var merger = Utils.GetMergerFromResources(
                Constants.ExercisesOrphanedConceptsResource,
                Constants.DesignOrphanedConceptsResource,
                exerciseResourceHandler
            );
            merger.MergeInLearningObjectives();
            Assert.NotEmpty(exerciseResourceHandler.ErrorResultJson);
            Assert.NotEqual("{\n  \"Errors\": []\n}",exerciseResourceHandler.ErrorResultJson);
        }

        [Fact]
        public void Merge_LearningObjectivesWithMatchingConcepts_ReportsNoErrors()
        {
            var exerciseResourceHandler = new ExerciseResourceHandler(
                Constants.ExercisesUnorphanedConceptsResource);
            var merger = Utils.GetMergerFromResources(
                Constants.ExercisesUnorphanedConceptsResource,
                Constants.DesignUnorphanedConceptsResource,
                exerciseResourceHandler
            );
            merger.MergeInLearningObjectives();
            Assert.Equal("{\n  \"Errors\": []\n}",exerciseResourceHandler.ErrorResultJson);
        }
    }
}