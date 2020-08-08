using System.Linq;
using System.Text.RegularExpressions;
using Xunit;

namespace ExerciseReport.Tests
{
    public class ExerciseReportTests
    {
        [Fact]
        public void Merge_DataWithFatalError_WritesNoExercises()
        {
            var exerciseResourceHandler = new ExerciseResourceHandler(
                Constants.DesignBrokenConceptsResource);
            var merger = Utils.GetMergerFromResources(
                Constants.DesignBrokenConceptsResource,
                Constants.ExercisesResource,
                exerciseResourceHandler);
            merger.MergeInLearningObjectives();
            Assert.Empty(exerciseResourceHandler.ExerciseResultJson);
            Assert.NotEmpty(exerciseResourceHandler.ErrorResultJson);
            Assert.NotEqual("{\n  \"Errors\": []\n}",exerciseResourceHandler.ErrorResultJson);
        }

        [Fact]
        public void Merge_ValidExerciseFile_ReportsNoErrors()
        {
            var exerciseResourceHandler = new ExerciseResourceHandler(
                Constants.ExercisesGoodResource);
            var merger = Utils.GetMergerFromResources(
                Constants.ExercisesGoodResource,
                Constants.SampleDesignResource,
                exerciseResourceHandler);
            merger.MergeInLearningObjectives();
            Assert.NotEmpty(exerciseResourceHandler.ExerciseResultJson);
            Assert.Equal("{\n  \"Errors\": []\n}",exerciseResourceHandler.ErrorResultJson);
        }

        [Fact]
        public void Merge_MixedExerciseFile_ReportsErrorsAndWritesExercises()
        {
            var exerciseResourceHandler = new ExerciseResourceHandler(
                Constants.ExercisesMixedResource);
            var merger = Utils.GetMergerFromResources(
                Constants.ExercisesMixedResource,
                Constants.SampleDesignResource,
                exerciseResourceHandler);
            merger.MergeInLearningObjectives();
            Assert.NotEmpty(exerciseResourceHandler.ExerciseResultJson);
            Assert.NotEmpty(exerciseResourceHandler.ErrorResultJson);
            Assert.NotEqual("{\n  \"Errors\": []\n}",exerciseResourceHandler.ErrorResultJson);
        }

        [Fact]
        public void Report_OnExerciseTree_ProducesWellFormedReport()
        {
            var rr = new ReportFormatter(PathNames.Default.Root);
            var merger = Utils.TestMergerWithResources;
            // var reportCollator = ReportCollator.CSharpReportCollator;
            // var merger = ExerciseMerger.TestMergerWithFileSystem;
            var exerciseFile = merger.Merge();
            var output = rr.CreateReport(exerciseFile.exerciseObjectTree);
            
            Assert.NotEmpty(output);
        }

        [Fact]
        public void Report_OnSimpleExerciseTree_ProducesWellFormedReportK()
        {
            var rr = new ReportFormatter(PathNames.Default.Root);
            string actual = rr.CreateReport(ExerciseTestData.Exercises["simple"]);
            string expected = Utils.GetResourceAsString("report_simple.md");
            Assert.Equal(expected, actual);
        }
    }
}