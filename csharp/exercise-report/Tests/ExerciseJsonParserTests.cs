using Xunit;

namespace ExerciseReport.Tests
{
    public class ExerciseJsonParserTests
    {
        [Fact]
        public void ParseExerciseJson_WithInvalidDocType_ReportsFatalError()
        {
            var ejp = new ExerciseJsonParser();
            var json = ExerciseReportTests.GetResourceAsString(
                Constants.ExercisesBadDocTypeResource);
            var results = ejp.FromString(json); 
            Assert.Equal(Result.FatalError, results.result);
            Assert.Contains(nameof(Exercise.DocumentType), results.errors[0].Message);
        }

        [Fact]
        public void ParseExerciseJson_WithInvalidLevel_ReportFatalsError()
        {
            var ejp = new ExerciseJsonParser();
            var json = ExerciseReportTests.GetResourceAsString(
                Constants.ExercisesBadLevelResource);
            var results = ejp.FromString(json); 
            Assert.Equal(Result.FatalError, results.result);
            Assert.Contains(nameof(Exercise.Level), results.errors[0].Message);
        }
        
        [Fact]
        public void ParseExerciseJson_WithWrongStructure_ReportsFatalError()
        {
            var ejp = new ExerciseJsonParser();
            var json = ExerciseReportTests.GetResourceAsString(
                Constants.ExercisesWrongStructureResource);
            var results = ejp.FromString(json); 
            Assert.Equal(Result.FatalError, results.result);
            Assert.Contains(nameof(ExerciseObjectTree), results.errors[0].Message);
        }
        
        [Fact]
        public void ParseExerciseJson_WithNonJsonSyntax_ReportsFatalError()
        {
            var ejp = new ExerciseJsonParser();
            var json = ExerciseReportTests.GetResourceAsString(
                Constants.ManyDesignsResource);
            var results = ejp.FromString(json); 
            Assert.Equal(Result.FatalError, results.result);
            Assert.Contains("'#' is an invalid start", results.errors[0].Message);
        }

        [Fact]
        public void ParseExerciseJson_WithSlightlyWrongStructure_ReportsFatalError()
        {
            var ejp = new ExerciseJsonParser();
            var json = ExerciseReportTests.GetResourceAsString(
                Constants.ExercisesSlightlyWrongResource);
            var results = ejp.FromString(json); 
            Assert.Equal(Result.FatalError, results.result);
            Assert.Contains("Json parser failed", results.errors[0].Message);
        }

        [Fact]
        public void ParseExerciseJson_WithMisssingFields_ReportsErrors()
        {
            var ejp = new ExerciseJsonParser();
            var json = ExerciseReportTests.GetResourceAsString(
                Constants.ExercisesMissingFieldsResource);
            var results = ejp.FromString(json); 
            Assert.Equal(Result.Errors, results.result);
            // Assert.Contains("Json parser failed", results.errors[0].Message);
        }
    }
}