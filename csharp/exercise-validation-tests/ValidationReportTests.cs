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
        public void Report_ItemsNotInTrackConfig_ProducesReport()
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