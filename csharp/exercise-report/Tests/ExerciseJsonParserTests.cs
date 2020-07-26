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
        public void ParseExerciseJson_WithMissingFields_ReportsErrors()
        {
            var ejp = new ExerciseJsonParser();
            var json = ExerciseReportTests.GetResourceAsString(
                Constants.ExercisesMissingFieldsResource);
            var results = ejp.FromString(json); 
            Assert.Equal(Result.Errors, results.result);
            Assert.Equal(5, results.errors.Count);
        }

        [Fact]
        public void ParseExerciseJson_WithMissingSlug_ReportsErrors()
        {
            var ejp = new ExerciseJsonParser();
            var json = ExerciseReportTests.GetResourceAsString(
                Constants.ExercisesMissingSlugResource);
            var results = ejp.FromString(json); 
            Assert.Equal(Result.Errors, results.result);
            Assert.Single(results.errors);
            Assert.Contains("slug", results.errors[0].Message);
        }

        [Fact]
        public void ParseExerciseJson_WithMissingLevel_ReportsErrors()
        {
            var ejp = new ExerciseJsonParser();
            var json = ExerciseReportTests.GetResourceAsString(
                Constants.ExercisesMissingLevelResource);
            var results = ejp.FromString(json); 
            Assert.Equal(Result.Errors, results.result);
            Assert.Single(results.errors);
            Assert.Contains("level", results.errors[0].Message);
        }

        [Fact]
        public void ParseExerciseJson_WithMissingDocType_ReportsErrors()
        {
            var ejp = new ExerciseJsonParser();
            var json = ExerciseReportTests.GetResourceAsString(
                Constants.ExercisesMissingDocumentTypeResource);
            var results = ejp.FromString(json); 
            Assert.Equal(Result.Errors, results.result);
            Assert.Single(results.errors);
            Assert.Contains("document-type", results.errors[0].Message);
        }

        [Fact]
        public void ParseExerciseJson_WithMissingDocLink_ReportsErrors()
        {
            var ejp = new ExerciseJsonParser();
            var json = ExerciseReportTests.GetResourceAsString(
                Constants.ExercisesMissingDocumentLinkResource);
            var results = ejp.FromString(json); 
            Assert.Equal(Result.Errors, results.result);
            Assert.Single(results.errors);
            Assert.Contains("document-link", results.errors[0].Message);
        }

        [Fact]
        public void ParseExerciseJson_WithMissingConcepts_ReportsErrors()
        {
            var ejp = new ExerciseJsonParser();
            var json = ExerciseReportTests.GetResourceAsString(
                Constants.ExercisesMissingConceptsResource);
            var results = ejp.FromString(json); 
            Assert.Equal(Result.Errors, results.result);
            Assert.Single(results.errors);
            Assert.Contains("concepts", results.errors[0].Message);
        }

        [Fact]
        public void ParseExerciseJson_WithMissingConceptName_ReportsErrors()
        {
            var ejp = new ExerciseJsonParser();
            var json = ExerciseReportTests.GetResourceAsString(
                Constants.ExercisesMissingConceptNameResource);
            var results = ejp.FromString(json); 
            Assert.Equal(Result.Errors, results.result);
            Assert.Single(results.errors);
            Assert.Contains("concept.name", results.errors[0].Message);
        }

        [Fact]
        public void ParseExerciseJson_ForEmptyString_ReportsErrors()
        {
            var ejp = new ExerciseJsonParser();
            var json = ExerciseReportTests.GetResourceAsString(
                Constants.ExercisesEmptyResource);
            var results = ejp.FromString(json); 
            Assert.Equal(Result.FatalError, results.result);
            Assert.Single(results.errors);
            Assert.Contains("does not contain any JSON tokens", results.errors[0].Message);
        }
    }
}