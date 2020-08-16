using System;
using Xunit;

namespace ExerciseReport.Tests
{
    public class ExerciseReportTests
    {
        private readonly ErrorResourceHandler testErrorResourceHandler;
        private readonly ErrorWriter testErrorWriter;
        private readonly FakeReportFileHandler fakeReportHandler;
        private readonly ReportWriter reportWriter;

        public ExerciseReportTests()
        {
            testErrorResourceHandler
                = new ErrorResourceHandler();
            testErrorWriter =
                new ErrorWriter(
                    testErrorResourceHandler,
                    new ErrorJsonParser());

            fakeReportHandler = new FakeReportFileHandler();
            reportWriter
                = new ReportWriter(fakeReportHandler,
                    new ReportFormatter());
        }
        [Fact]
        public void Merge_DataWithFatalError_WritesNoExercises()
        {
            bool exceptionThrown = false;
            var merger = Utils.GetMergerFromResources(
                Constants.ExercisesResource,
                Constants.DesignBrokenConceptsResource
                );
            try
            {
                new ReportProcessor().Process(merger, reportWriter, testErrorWriter);

            }
            catch (Exception e)
            {
                var _ = e;
                exceptionThrown = true;
            }
            Assert.True(exceptionThrown);
            Assert.Empty(fakeReportHandler.Report);
            Assert.NotEmpty(testErrorResourceHandler.ResultJson);
            Assert.NotEqual("{\n  \"Errors\": []\n}", testErrorResourceHandler.ResultJson);
        }

        [Fact]
        public void Merge_ValidExerciseFile_ReportsNoErrors()
        {
            var merger = Utils.GetMergerFromResources(
                Constants.ExercisesGoodResource,
                Constants.SampleDesignResource);
            new ReportProcessor().Process(merger, reportWriter, testErrorWriter);
            Assert.NotEmpty(fakeReportHandler.Report);
            Assert.Equal("{\n  \"Errors\": []\n}", testErrorResourceHandler.ResultJson);
        }

        [Fact]
        public void Merge_MixedExerciseFile_ReportsErrorsAndWritesExercises()
        {
            var merger = Utils.GetMergerFromResources(
                Constants.ExercisesMixedResource,
                Constants.SampleDesignResource);
            new ReportProcessor().Process(merger, reportWriter, testErrorWriter);
            Assert.NotEmpty(fakeReportHandler.Report);
            Assert.NotEmpty(testErrorResourceHandler.ResultJson);
            Assert.NotEqual("{\n  \"Errors\": []\n}", testErrorResourceHandler.ResultJson);
        }

        [Fact]
        public void Report_OnExerciseTree_ProducesWellFormedReport()
        {
            var rr = new ReportFormatter();
            var merger = Utils.GetMergerFromResources(
                Constants.ExercisesResource,
                Constants.ManyDesignsResource);
            new ReportProcessor().Process(merger, reportWriter, testErrorWriter);
            
            Assert.NotEmpty(fakeReportHandler.Report);
        }

        [Fact]
        public void Report_OnSimpleExerciseTree_ProducesWellFormedReportK()
        {
            var rr = new ReportFormatter();
            string actual = rr.CreateReport(ExerciseTestData.Exercises["simple"]);
            string expected = Utils.GetResourceAsString(Constants.ReportSimpleResource);
            Assert.Equal(expected, actual);
        }
    }

    internal class FakeReportFileHandler : IReportFileHandler
    {
        public string Report { get; private set; } = string.Empty;
        
        public void WriteFile(string reportMarkdown)
        {
            Report = reportMarkdown;
        }
    }
}