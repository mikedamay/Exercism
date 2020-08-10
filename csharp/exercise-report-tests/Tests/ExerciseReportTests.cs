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
            (var merger, var exerciseResourceHandler) = Utils.GetMergerFromResourcesPlusHandler(
                Constants.DesignBrokenConceptsResource,
                Constants.ExercisesResource);
            merger.MergeInLearningObjectives();
            Assert.Empty(exerciseResourceHandler.ExerciseResultJson);
            Assert.NotEmpty(exerciseResourceHandler.ErrorResultJson);
            Assert.NotEqual("{\n  \"Errors\": []\n}",exerciseResourceHandler.ErrorResultJson);
        }

        [Fact]
        public void Merge_ValidExerciseFile_ReportsNoErrors()
        {
            (var merger, var exerciseResourceHandler) = Utils.GetMergerFromResourcesPlusHandler(
                Constants.ExercisesGoodResource,
                Constants.SampleDesignResource);
            merger.MergeInLearningObjectives();
            Assert.NotEmpty(exerciseResourceHandler.ExerciseResultJson);
            Assert.Equal("{\n  \"Errors\": []\n}",exerciseResourceHandler.ErrorResultJson);
        }

        [Fact]
        public void Merge_MixedExerciseFile_ReportsErrorsAndWritesExercises()
        {
            (var merger, var exerciseResourceHandler) = Utils.GetMergerFromResourcesPlusHandler(
                Constants.ExercisesMixedResource,
                Constants.SampleDesignResource);
            merger.MergeInLearningObjectives();
            Assert.NotEmpty(exerciseResourceHandler.ExerciseResultJson);
            Assert.NotEmpty(exerciseResourceHandler.ErrorResultJson);
            Assert.NotEqual("{\n  \"Errors\": []\n}",exerciseResourceHandler.ErrorResultJson);
        }

        [Fact]
        public void Report_OnExerciseTree_ProducesWellFormedReport()
        {
            var rr = new ReportFormatter(PathNames.Default.Root);
            (var merger, var exerciseResourceHandler) =  Utils.GetMergerFromResourcesPlusHandler(Constants.ExercisesResource,
                Constants.ManyDesignsResource);
            // var reportCollator = ReportCollator.CSharpReportCollator;
            // var merger = ExerciseMerger.TestMergerWithFileSystem;
            merger.MergeInLearningObjectives();
            var efc = new ExerciseFileCollator(
                new ExerciseResourceHandler(), new ExerciseJsonParser());
            var outputs = efc.ReadExercises();
            var output = rr.CreateReport(outputs.ExerciseObjectTree);
            
            Assert.NotEmpty(output);
        }

        [Fact]
        public void Report_OnSimpleExerciseTree_ProducesWellFormedReportK()
        {
            var rr = new ReportFormatter(PathNames.Default.Root);
            string actual = rr.CreateReport(ExerciseTestData.Exercises["simple"]);
            string expected = Utils.GetResourceAsString(Constants.ReportSimpleResource);
            Assert.Equal(expected, actual);
        }
    }
}