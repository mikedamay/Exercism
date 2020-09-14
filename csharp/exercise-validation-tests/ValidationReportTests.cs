using System.Collections.Generic;
using ExerciseReport;
using ExerciseValidation;
using Xunit;
using IReportFileHandler = ExerciseReport.IReportFileHandler;
using static ExerciseValidationTests.Utils;

namespace ExerciseValidationTests
{
    public class ValidationReportTests
    {
        private IErrorFileHandler testErrorResourceHandler;
        private ErrorWriter testErrorWriter;
        private FakeReportFileHandler fakeReportHandler;
        private ReportWriter reportWriter;

        public ValidationReportTests()
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
        public void Report_ItemsNotInTrackConfig_ReportsItemsNotInTrackConfigOnly()
        {
            reportWriter.WriteReport(
                new List<ExerciseAndConcept>(),
                new List<ExerciseAndConcept>
                {
                    new ExerciseAndConcept("exercise1", "concept1"),
                    new ExerciseAndConcept("exercise2", "concept2"),
                }
            );
            Assert.Equal(GetResourceAsString(
                    Constants.NotInTrackConfigResource)
                , fakeReportHandler.Report);
        }

        [Fact]
        public void Report_ItemsNotInExerciseReport_ReportsItemsNotInExerciseReportOnly()
        {
            reportWriter.WriteReport(
                new List<ExerciseAndConcept>
                {
                    new ExerciseAndConcept("exercise1", "concept1"),
                    new ExerciseAndConcept("exercise2", "concept2"),
                },
                new List<ExerciseAndConcept>()
            );
            Assert.Equal(GetResourceAsString(
                    Constants.NotInExerciseReportResource)
                , fakeReportHandler.Report);
        }

        [Fact]
        public void Report_ItemsNotInBoth_ReportsItemsNotInBoth()
        {
            reportWriter.WriteReport(
                new List<ExerciseAndConcept>
                {
                    new ExerciseAndConcept("exercise1", "concept1"),
                    new ExerciseAndConcept("exercise2", "concept2"),
                },
                new List<ExerciseAndConcept>
                {
                    new ExerciseAndConcept("exercise3", "concept3"),
                    new ExerciseAndConcept("exercise4", "concept4"),
                }
            );
            Assert.Equal(GetResourceAsString(
                    Constants.NotInBothResource)
                , fakeReportHandler.Report);
        }

        [Fact]
        public void Report_ItemsNotInEither_ReportsItemsNotInEither()
        {
            reportWriter.WriteReport(
                new List<ExerciseAndConcept>(),
                new List<ExerciseAndConcept>()
            );
            Assert.Equal(GetResourceAsString(
                    Constants.NotInEitherResource)
                , fakeReportHandler.Report);
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