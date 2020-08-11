using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using Xunit;

namespace ExerciseReport.Tests
{
    public class ExerciseReportTests
    {
        private static ErrorResourceHandler testErrorResourceHandler
            = new ErrorResourceHandler();
        private static ErrorWriter CSharpTestErrorWriter { get; } =
            new ErrorWriter(
                testErrorResourceHandler, 
                new ErrorJsonParser());

        [Fact]
        public void Merge_DataWithFatalError_WritesNoExercises()
        {
            (var merger, var exerciseResourceHandler) = Utils.GetMergerFromResourcesPlusHandler(
                Constants.DesignBrokenConceptsResource,
                Constants.ExercisesResource);
            WriteMergeResults(merger.MergeExercisesAndLearningObjectives(), exerciseResourceHandler);
            Assert.Empty(exerciseResourceHandler.ExerciseResultJson);
            Assert.NotEmpty(testErrorResourceHandler.ResultJson);
            Assert.NotEqual("{\n  \"Errors\": []\n}", testErrorResourceHandler.ResultJson);
        }

        [Fact]
        public void Merge_ValidExerciseFile_ReportsNoErrors()
        {
            (var merger, var exerciseResourceHandler) = Utils.GetMergerFromResourcesPlusHandler(
                Constants.ExercisesGoodResource,
                Constants.SampleDesignResource);
            WriteMergeResults(merger.MergeExercisesAndLearningObjectives(), exerciseResourceHandler);
            Assert.NotEmpty(exerciseResourceHandler.ExerciseResultJson);
            Assert.Equal("{\n  \"Errors\": []\n}", testErrorResourceHandler.ResultJson);
        }

        [Fact]
        public void Merge_MixedExerciseFile_ReportsErrorsAndWritesExercises()
        {
            (var merger, var exerciseResourceHandler) = Utils.GetMergerFromResourcesPlusHandler(
                Constants.ExercisesMixedResource,
                Constants.SampleDesignResource);
            WriteMergeResults(merger.MergeExercisesAndLearningObjectives(), exerciseResourceHandler);
            Assert.NotEmpty(exerciseResourceHandler.ExerciseResultJson);
            Assert.NotEmpty(testErrorResourceHandler.ResultJson);
            Assert.NotEqual("{\n  \"Errors\": []\n}", testErrorResourceHandler.ResultJson);
        }

        [Fact]
        public void Report_OnExerciseTree_ProducesWellFormedReport()
        {
            var rr = new ReportFormatter(PathNames.Default.Root);
            (var merger, var exerciseResourceHandler) =  Utils.GetMergerFromResourcesPlusHandler(Constants.ExercisesResource,
                Constants.ManyDesignsResource);
            // var reportCollator = ReportReader.CSharpReportReader;
            // var merger = ExerciseMerger.TestMergerWithFileSystem;
            WriteMergeResults(merger.MergeExercisesAndLearningObjectives(), exerciseResourceHandler);
            var efc = new ExerciseReader(
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


        private void WriteMergeResults(
            (Result Result, ExerciseObjectTree ExerciseObjectTree, IList<Error> Errors) mergeResults,
            ExerciseResourceHandler exerciseResourceHandler)
        {
            var errorWriter = CSharpTestErrorWriter;
            errorWriter.Write(mergeResults.Errors);
            if (mergeResults.Result != Result.FatalError)
            {
                var exerciseJsonParser = new ExerciseJsonParser();
                var json = exerciseJsonParser.ToString(mergeResults.ExerciseObjectTree);
                exerciseResourceHandler.WriteExerciseFile(json);
            }
        }
    }
}